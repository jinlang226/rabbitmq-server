%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2026 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.
%%

-module(rabbit_mgmt_wm_feature_flag_enable).

-export([init/2,
         content_types_accepted/2, is_authorized/2,
         allowed_methods/2, accept_content/2]).
-export([variances/2]).

-include_lib("rabbitmq_management_agent/include/rabbit_mgmt_records.hrl").
%%--------------------------------------------------------------------

init(Req, _Args) ->
    {cowboy_rest,
     rabbit_mgmt_headers:set_common_permission_headers(Req, ?MODULE),
     #context{}}.

variances(Req, Context) ->
    {[<<"accept-encoding">>, <<"origin">>], Req, Context}.

content_types_accepted(ReqData, Context) ->
    {[{{<<"application">>, <<"json">>, '*'}, accept_content}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {[<<"PUT">>, <<"OPTIONS">>], ReqData, Context}.

is_authorized(ReqData, Context) ->
    rabbit_mgmt_util:is_authorized_admin(ReqData, Context).

accept_content(ReqData, #context{} = Context) ->
    NameS = rabbit_mgmt_util:id(name, ReqData),
    Before = feature_flags_snapshot(),
    try
        Name = list_to_existing_atom(binary_to_list(NameS)),
        case rabbit_feature_flags:enable(Name) of
            ok ->
                emit_trace(ReqData, NameS, Before, feature_flags_snapshot(), true, ok),
                {true, ReqData, Context};
            {error, Reason1} ->
                emit_trace(ReqData, NameS, Before, feature_flags_snapshot(), false, Reason1),
                FormattedReason1 = rabbit_ff_extra:format_error(Reason1),
                rabbit_mgmt_util:bad_request(
                  list_to_binary(FormattedReason1), ReqData, Context)
        end
    catch
        _:badarg ->
            Reason2 = unsupported,
            emit_trace(ReqData, NameS, Before, feature_flags_snapshot(), false, Reason2),
            FormattedReason2 = rabbit_ff_extra:format_error(Reason2),
            rabbit_mgmt_util:bad_request(
              list_to_binary(FormattedReason2), ReqData, Context)
    end.

feature_flags_snapshot() ->
    case rabbit_feature_flags:list(enabled) of
        Map when is_map(Map) ->
            Names = lists:sort([atom_to_binary(Name, utf8) || Name <- maps:keys(Map)]),
            #{
                <<"enabledFeatureFlagCount">> => length(Names),
                <<"enabledFeatureFlags">> => Names
            };
        Other ->
            #{<<"raw">> => list_to_binary(io_lib:format("~tp", [Other]))}
    end.

emit_trace(ReqData, FeatureFlag, Before, After, Success, Reason) ->
    EventType =
        case Success of
            true -> <<"FeatureFlagsEnabled">>;
            false -> <<"EnableFeatureFlagsFailed">>
        end,
    rabbit_mgmt_trace_logger:emit(
      EventType,
      Before,
      After,
      #{<<"success">> => Success, <<"reason">> => to_bin(Reason)},
      maps:merge(#{
          <<"featureFlag">> => FeatureFlag,
          <<"source">> => <<"management_api">>,
          <<"reason">> => to_bin(Reason)
      }, trace_context(ReqData))).

trace_context(ReqData) ->
    add_if_defined(
      <<"traceEnabled">>,
      parse_bool(cowboy_req:header(<<"x-rabbitmq-trace-enabled">>, ReqData)),
      add_if_defined(
        <<"testCase">>,
        cowboy_req:header(<<"x-rabbitmq-trace-testcase">>, ReqData),
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

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_bin(V) -> list_to_binary(io_lib:format("~tp", [V])).
