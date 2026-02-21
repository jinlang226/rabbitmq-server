%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2026 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.
%%

-module(rabbit_mgmt_wm_queue_get).

-export([init/2, resource_exists/2, is_authorized/2, allow_missing_post/2,
  allowed_methods/2, accept_content/2, content_types_provided/2,
  content_types_accepted/2]).
-export([variances/2]).

-include_lib("rabbitmq_management_agent/include/rabbit_mgmt_records.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%%--------------------------------------------------------------------

init(Req, _State) ->
    {cowboy_rest, rabbit_mgmt_headers:set_common_permission_headers(Req, ?MODULE), #context{}}.

variances(Req, Context) ->
    {[<<"accept-encoding">>, <<"origin">>], Req, Context}.

allowed_methods(ReqData, Context) ->
    {[<<"POST">>, <<"OPTIONS">>], ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {rabbit_mgmt_util:responder_map(to_json), ReqData, Context}.

resource_exists(ReqData0, Context) ->
    case rabbit_mgmt_wm_queue:queue(ReqData0) of
        not_found ->
            ReqData1 = rabbit_mgmt_util:set_resp_not_found(<<"queue_not_found">>, ReqData0),
            {false, ReqData1, Context};
        _ ->
            {true, ReqData0, Context}
    end.

allow_missing_post(ReqData, Context) ->
    {false, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
   {[{'*', accept_content}], ReqData, Context}.

accept_content(ReqData, Context) ->
    rabbit_mgmt_util:post_respond(do_it(ReqData, Context)).

do_it(ReqData0, Context) ->
    VHost = rabbit_mgmt_util:vhost(ReqData0),
    Q = rabbit_mgmt_util:id(queue, ReqData0),
    TraceContext = trace_context(ReqData0),
    rabbit_mgmt_util:with_decode(
      [ackmode, count, encoding], ReqData0, Context,
      fun([AckModeBin, CountBin, EncBin], Body, ReqData) ->
              rabbit_mgmt_util:with_channel(
                VHost, ReqData, Context,
                fun (Ch) ->
                        Before = queue_snapshot(VHost, Q),
                        AckMode = list_to_atom(binary_to_list(AckModeBin)),
                        Count = rabbit_mgmt_util:parse_int(CountBin),
                        Enc = case EncBin of
                                  <<"auto">>   -> auto;
                                  <<"base64">> -> base64;
                                  _            -> throw({error, <<"Unsupported encoding. Please use auto or base64.">>})
                              end,
                        Trunc = case maps:get(truncate, Body, undefined) of
                                    undefined -> none;
                                    TruncBin  -> rabbit_mgmt_util:parse_int(
                                                   TruncBin)
                                end,

                        Reply = basic_gets(Count, Ch, Q, AckMode, Enc, Trunc),
                        maybe_return(Reply, Ch, AckMode),
                        After = queue_snapshot(VHost, Q),
                        rabbit_mgmt_trace_logger:emit(
                          <<"ConsumeMessage">>,
                          Before,
                          After,
                          #{
                              <<"success">> => true,
                              <<"hasMessage">> => (length(Reply) > 0),
                              <<"consumedCount">> => length(Reply)
                          },
                          maps:merge(#{
                              <<"source">> => <<"management_api">>,
                              <<"queue">> => Q,
                              <<"ackMode">> => AckModeBin,
                              <<"requestedCount">> => Count,
                              <<"encoding">> => EncBin,
                              <<"reason">> => <<"ok">>
                          }, TraceContext)),
                        rabbit_mgmt_util:reply(remove_delivery_tag(Reply),
					       ReqData, Context)
                end)
      end).




basic_gets(0, _, _, _, _, _) ->
    [];

basic_gets(Count, Ch, Q, AckMode, Enc, Trunc) ->
    case basic_get(Ch, Q, AckMode, Enc, Trunc) of
        none -> [];
        M    -> [M | basic_gets(Count - 1, Ch, Q, AckMode, Enc, Trunc)]
    end.



ackmode_to_requeue(reject_requeue_false) -> false;
ackmode_to_requeue(reject_requeue_true) -> true.

parse_ackmode(ack_requeue_false) -> true;
parse_ackmode(ack_requeue_true) -> false;
parse_ackmode(reject_requeue_false) -> false;
parse_ackmode(reject_requeue_true) -> false.


% the messages must rejects later,
% because we get always the same message if the
% messages are requeued inside basic_get/5
maybe_return(R, Ch, AckMode) ->
    lists:foreach(fun(X) ->
                          maybe_reject_or_nack(Ch, AckMode,
                                               proplists:get_value(delivery_tag, X))
                  end, R).

% removes the delivery_tag from the reply.
% it is not necessary
remove_delivery_tag([])    -> [];
remove_delivery_tag([H|T]) ->
    [proplists:delete(delivery_tag, H) | [X || X <- remove_delivery_tag(T)]].


maybe_reject_or_nack(Ch, AckMode, DeliveryTag)
  when AckMode == reject_requeue_true;
       AckMode == reject_requeue_false ->
    amqp_channel:call(Ch,
                      #'basic.reject'{delivery_tag = DeliveryTag,
                                      requeue = ackmode_to_requeue(AckMode)});
maybe_reject_or_nack(Ch, ack_requeue_true, DeliveryTag) ->
    amqp_channel:call(Ch,
                      #'basic.nack'{delivery_tag = DeliveryTag,
                                    multiple = false,
                                    requeue = true});
maybe_reject_or_nack(_Ch, _AckMode, _DeliveryTag) -> ok.


basic_get(Ch, Q, AckMode, Enc, Trunc) ->
    case amqp_channel:call(Ch,
			   #'basic.get'{queue = Q,
					no_ack = parse_ackmode(AckMode)}) of
        {#'basic.get_ok'{redelivered   = Redelivered,
                         exchange      = Exchange,
                         routing_key   = RoutingKey,
                         message_count = MessageCount,
                         delivery_tag  = DeliveryTag},
         #amqp_msg{props = Props, payload = Payload}} ->
            [{payload_bytes, size(Payload)},
             {redelivered,   Redelivered},
             {exchange,      Exchange},
             {routing_key,   RoutingKey},
             {message_count, MessageCount},
             {delivery_tag,  DeliveryTag},
             {properties,    rabbit_mgmt_format:basic_properties(Props)}] ++
                payload_part(maybe_truncate(Payload, Trunc), Enc);
        #'basic.get_empty'{} ->
            none
    end.


is_authorized(ReqData, Context) ->
    rabbit_mgmt_util:is_authorized_vhost(ReqData, Context).

%%--------------------------------------------------------------------

maybe_truncate(Payload, none)                         -> Payload;
maybe_truncate(Payload, Len) when size(Payload) < Len -> Payload;
maybe_truncate(Payload, Len) ->
    <<Start:Len/binary, _Rest/binary>> = Payload,
    Start.

payload_part(Payload, Enc) ->
    {PL, E} = case Enc of
                  auto -> case is_utf8(Payload) of
                              true -> {Payload, string};
                              false -> {base64:encode(Payload), base64}
                          end;
                  _    -> {base64:encode(Payload), base64}
              end,
    [{payload, PL}, {payload_encoding, E}].

is_utf8(<<>>) -> true;
is_utf8(<<_/utf8, Rest/bits>>) -> is_utf8(Rest);
is_utf8(_) -> false.

queue_snapshot(VHost, QueueName) ->
    Name = rabbit_misc:r(VHost, queue, QueueName),
    case rabbit_amqqueue:lookup(Name) of
        {ok, Q} ->
            Info = rabbit_amqqueue:info(Q, [messages, messages_ready, messages_unacknowledged, consumers]),
            #{
                <<"queueFound">> => true,
                <<"queue">> => QueueName,
                <<"queueInfo">> => info_map(Info)
            };
        {error, not_found} ->
            #{
                <<"queueFound">> => false,
                <<"queue">> => QueueName
            }
    end.

info_map(Info) ->
    maps:from_list([{to_bin(K), normalize(Val)} || {K, Val} <- Info]).

normalize(V) when is_integer(V); is_float(V); is_boolean(V) -> V;
normalize(V) when is_binary(V) -> V;
normalize(V) when is_atom(V) -> atom_to_binary(V, utf8);
normalize(V) when is_list(V) ->
    case io_lib:printable_unicode_list(V) of
        true -> list_to_binary(V);
        false -> [normalize(E) || E <- V]
    end;
normalize(V) -> to_bin(V).

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_bin(V) when is_list(V) -> list_to_binary(V);
to_bin(V) -> list_to_binary(io_lib:format("~tp", [V])).

trace_context(Req) ->
    add_if_defined(
      <<"traceEnabled">>,
      parse_bool(cowboy_req:header(<<"x-rabbitmq-trace-enabled">>, Req)),
      add_if_defined(
        <<"testCase">>,
        cowboy_req:header(<<"x-rabbitmq-trace-testcase">>, Req),
        #{})).

add_if_defined(_K, undefined, M) -> M;
add_if_defined(K, V, M) -> M#{K => V}.

parse_bool(undefined) -> undefined;
parse_bool(<<"1">>) -> true;
parse_bool(<<"true">>) -> true;
parse_bool(<<"yes">>) -> true;
parse_bool(<<"on">>) -> true;
parse_bool(<<"0">>) -> false;
parse_bool(<<"false">>) -> false;
parse_bool(<<"no">>) -> false;
parse_bool(<<"off">>) -> false;
parse_bool(Other) -> to_bin(Other).
