%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2026 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.
%%

-module(rabbit_mgmt_wm_rebalance_queues).

-export([init/2, service_available/2, resource_exists/2,
         content_types_provided/2, content_types_accepted/2,
         is_authorized/2, allowed_methods/2, accept_content/2]).
-export([variances/2]).

-include_lib("rabbitmq_management_agent/include/rabbit_mgmt_records.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------

init(Req, [Mode]) ->
    Headers = rabbit_mgmt_headers:set_common_permission_headers(Req, ?MODULE),
    {cowboy_rest, Headers, {Mode, #context{}}}.

service_available(Req, {{queues, all}, _Context}=State) ->
    {true, Req, State};
service_available(Req, State) ->
    {false, Req, State}.

variances(Req, State) ->
    {[<<"accept-encoding">>, <<"origin">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"plain">>, '*'}, undefined}], Req, State}.

content_types_accepted(Req, State) ->
   {[{'*', accept_content}], Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

accept_content(Req, {_Mode, #context{user = #user{username = Username}}}=State) ->
    Before = cluster_snapshot(),
    TraceContext = trace_context(Req),
    try
        ?LOG_INFO("User '~ts' has initiated a queue rebalance", [Username]),
        spawn(fun() ->
            rabbit_amqqueue:rebalance(all, <<".*">>, <<".*">>)
        end),
        rabbit_mgmt_trace_logger:emit(
          <<"QueueRebalanceCommand">>,
          Before,
          cluster_snapshot(),
          #{<<"success">> => true},
          maps:merge(#{
              <<"source">> => <<"management_api">>,
              <<"username">> => normalize_username(Username)
          }, TraceContext)),
        {true, Req, State}
    catch
        {error, Reason} ->
            rabbit_mgmt_trace_logger:emit(
              <<"QueueRebalanceCommand">>,
              Before,
              cluster_snapshot(),
              #{<<"success">> => false, <<"reason">> => to_bin(Reason)},
              maps:merge(#{
                  <<"source">> => <<"management_api">>,
                  <<"username">> => normalize_username(Username)
              }, TraceContext)),
            rabbit_mgmt_util:bad_request(iolist_to_binary(Reason), Req, State)
    end.

is_authorized(Req0, {Mode, Context0}) ->
    {Res, Req1, Context1} = rabbit_mgmt_util:is_authorized_admin(Req0, Context0),
    {Res, Req1, {Mode, Context1}}.

cluster_snapshot() ->
    case rabbit_nodes:list_running() of
        Nodes when is_list(Nodes) ->
            NodeBins = lists:sort([to_bin(Node) || Node <- Nodes]),
            #{
                <<"runningNodeCount">> => length(NodeBins),
                <<"runningNodes">> => NodeBins
            };
        Other ->
            #{<<"raw">> => to_bin(Other)}
    end.

normalize_username(Username) when is_binary(Username) -> Username;
normalize_username(Username) when is_list(Username) -> list_to_binary(Username);
normalize_username(Username) -> to_bin(Username).

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_atom(V) -> atom_to_binary(V, utf8);
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
