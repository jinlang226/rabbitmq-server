%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2026 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.
%%

-module(rabbit_mgmt_wm_exchange_publish).

-export([init/2, resource_exists/2, allow_missing_post/2, is_authorized/2,
         allowed_methods/2,  content_types_provided/2, accept_content/2,
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
    case rabbit_mgmt_wm_exchange:exchange(ReqData0) of
        not_found ->
            ReqData1 = rabbit_mgmt_util:set_resp_not_found(<<"exchange_not_found">>, ReqData0),
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
    X = rabbit_mgmt_util:id(exchange, ReqData0),
    rabbit_mgmt_util:with_decode(
      [routing_key, properties, payload, payload_encoding], ReqData0, Context,
      fun ([RoutingKey, Props0, Payload0, Enc], _, ReqData) when is_binary(Payload0) ->
              rabbit_mgmt_util:with_channel(
                VHost, ReqData, Context,
                fun (Ch) ->
                        Before = publish_snapshot(VHost, X, RoutingKey),
                        MRef = erlang:monitor(process, Ch),
                        amqp_channel:register_confirm_handler(Ch, self()),
                        amqp_channel:register_return_handler(Ch, self()),
                        amqp_channel:call(Ch, #'confirm.select'{}),
                        Props = rabbit_mgmt_format:to_basic_properties(Props0),
                        Payload = decode(Payload0, Enc),
                        amqp_channel:cast(Ch, #'basic.publish'{
                                            exchange    = X,
                                            routing_key = RoutingKey,
                                            mandatory   = true},
                                          #amqp_msg{props   = Props,
                                                    payload = Payload}),
                        receive
                            {#'basic.return'{}, _} ->
                                receive
                                    #'basic.ack'{} -> ok
                                end,
                                trace_publish(Before, VHost, X, RoutingKey, Enc, size(Payload0), false, true, ok),
                                good(MRef, false, ReqData, Context);
                            #'basic.ack'{} ->
                                trace_publish(Before, VHost, X, RoutingKey, Enc, size(Payload0), true, true, ok),
                                good(MRef, true, ReqData, Context);
                            #'basic.nack'{} ->
                                erlang:demonitor(MRef),
                                trace_publish(Before, VHost, X, RoutingKey, Enc, size(Payload0), false, false, rejected),
                                bad(rejected, ReqData, Context);
                            {'DOWN', _, _, _, Err} ->
                                trace_publish(Before, VHost, X, RoutingKey, Enc, size(Payload0), false, false, Err),
                                bad(Err, ReqData, Context)
                        end
                end);
          ([_RoutingKey, _Props, _Payload, _Enc], _, _ReqData) ->
              throw({error, payload_not_string})
      end).

good(MRef, Routed, ReqData, Context) ->
    erlang:demonitor(MRef),
    rabbit_mgmt_util:reply([{routed, Routed}], ReqData, Context).

bad({shutdown, {connection_closing,
                {server_initiated_close, Code, Reason}}}, ReqData, Context) ->
    rabbit_mgmt_util:bad_request_exception(Code, Reason, ReqData, Context);

bad({shutdown, {server_initiated_close, Code, Reason}}, ReqData, Context) ->
    rabbit_mgmt_util:bad_request_exception(Code, Reason, ReqData, Context);
bad(rejected, ReqData, Context) ->
    Msg = "Unable to publish message. Check queue limits.",
    rabbit_mgmt_util:bad_request_exception(rejected, Msg, ReqData, Context);
bad({{coordinator_unavailable, _}, _}, ReqData, Context) ->
    Msg = "Unable to publish message. Coordinator unavailable.",
    rabbit_mgmt_util:bad_request_exception(rejected, Msg, ReqData, Context).

is_authorized(ReqData, Context) ->
    rabbit_mgmt_util:is_authorized_vhost(ReqData, Context).

%%--------------------------------------------------------------------

decode(Payload, <<"string">>) -> Payload;
decode(Payload, <<"base64">>) -> rabbit_mgmt_util:b64decode_or_throw(Payload);
decode(_Payload, Enc)         -> throw({error, {unsupported_encoding, Enc}}).

trace_publish(Before, VHost, Exchange, RoutingKey, Enc, PayloadBytes, Routed, Success, Reason) ->
    rabbit_mgmt_trace_logger:emit(
      <<"PublishMessage">>,
      Before,
      publish_snapshot(VHost, Exchange, RoutingKey),
      #{
          <<"success">> => Success,
          <<"routed">> => Routed,
          <<"reason">> => to_bin(Reason)
      },
      #{
          <<"source">> => <<"management_api">>,
          <<"exchange">> => Exchange,
          <<"routingKey">> => RoutingKey,
          <<"payloadEncoding">> => Enc,
          <<"payloadBytes">> => PayloadBytes
      }).

publish_snapshot(VHost, <<>>, RoutingKey) ->
    queue_snapshot(VHost, RoutingKey);
publish_snapshot(_VHost, Exchange, RoutingKey) ->
    #{
        <<"exchange">> => Exchange,
        <<"routingKey">> => RoutingKey
    }.

queue_snapshot(VHost, QueueName) ->
    Name = rabbit_misc:r(VHost, queue, QueueName),
    case rabbit_amqqueue:lookup(Name) of
        {ok, Q} ->
            Info = rabbit_amqqueue:info(Q, [messages, messages_ready, messages_unacknowledged, consumers]),
            #{<<"queueFound">> => true, <<"queue">> => QueueName, <<"queueInfo">> => info_map(Info)};
        {error, not_found} ->
            #{<<"queueFound">> => false, <<"queue">> => QueueName}
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
