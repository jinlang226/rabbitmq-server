%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2026 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.

-module(rabbit_mgmt_trace_logger).

-export([emit/5]).

-define(ENABLED_ENV, "RABBITMQ_APP_TRACE_ENABLED").
-define(DIR_ENV, "RABBITMQ_APP_TRACE_DIR").
-define(TESTCASE_ENV, "RABBITMQ_APP_TRACE_TESTCASE").

emit(EventType, Before, After, Result, Details0) ->
    Details = normalize(Details0),
    case enabled(Details) of
        true ->
            TestCase = testcase_from_details(Details),
            Path = trace_path(TestCase),
            Doc0 = load_doc(Path, TestCase),
            Events0 = get_events(Doc0),
            Event = #{
                <<"stepSeq">> => length(Events0) + 1,
                <<"timestamp">> => timestamp(),
                <<"eventType">> => normalize(EventType),
                <<"before">> => normalize(Before),
                <<"after">> => normalize(After),
                <<"result">> => normalize(Result),
                <<"details">> => Details
            },
            Doc = Doc0#{
                <<"schemaVersion">> => 1,
                <<"testCase">> => normalize(TestCase),
                <<"events">> => Events0 ++ [Event]
            },
            filelib:ensure_dir(Path),
            file:write_file(Path, rabbit_json:encode(Doc));
        false ->
            ok
    end,
    ok.

enabled(Details) ->
    case maps:get(<<"traceEnabled">>, Details, undefined) of
        true -> true;
        <<"true">> -> true;
        <<"1">> -> true;
        _ ->
            case string:lowercase(os:getenv(?ENABLED_ENV, "false")) of
                "1" -> true;
                "true" -> true;
                "yes" -> true;
                "on" -> true;
                _ -> false
            end
    end.

trace_path(TestCase) ->
    Dir = os:getenv(?DIR_ENV, "."),
    filename:join(Dir, TestCase ++ ".json").

testcase_from_details(Details) ->
    case maps:get(<<"testCase">>, Details, undefined) of
        undefined -> sanitize_testcase(default_testcase());
        V when is_binary(V) -> sanitize_testcase(binary_to_list(V));
        V when is_list(V) -> sanitize_testcase(V);
        V -> sanitize_testcase(io_lib:format("~tp", [V]))
    end.

default_testcase() ->
    Raw0 = os:getenv(?TESTCASE_ENV, "manual"),
    string:trim(Raw0).

sanitize_testcase(Str0) ->
    Str = string:trim(Str0),
    Sanitized = re:replace(Str, "[^A-Za-z0-9_.-]", "_", [global, {return, list}]),
    case Sanitized of
        "" -> "manual";
        _ -> Sanitized
    end.

load_doc(Path, TestCase) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            case rabbit_json:decode(Bin) of
                Decoded when is_map(Decoded) -> Decoded;
                _ -> base_doc(TestCase)
            end;
        _ ->
            base_doc(TestCase)
    end.

base_doc(TestCase) ->
    #{
        <<"schemaVersion">> => 1,
        <<"testCase">> => normalize(TestCase),
        <<"events">> => []
    }.

get_events(Doc) ->
    case maps:get(<<"events">>, Doc, []) of
        L when is_list(L) -> L;
        _ -> []
    end.

timestamp() ->
    try
        list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(millisecond), [{unit, millisecond}]))
    catch
        _:_ ->
            list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second), [{unit, second}]))
    end.

normalize(V) when is_integer(V); is_float(V); is_boolean(V); V =:= null ->
    V;
normalize(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
normalize(V) when is_binary(V) ->
    V;
normalize(V) when is_list(V) ->
    case io_lib:printable_unicode_list(V) of
        true -> list_to_binary(V);
        false -> [normalize(E) || E <- V]
    end;
normalize(V) when is_tuple(V) ->
    [normalize(E) || E <- tuple_to_list(V)];
normalize(V) when is_map(V) ->
    maps:from_list([
        {normalize_map_key(K), normalize(Val)} || {K, Val} <- maps:to_list(V)
    ]);
normalize(undefined) ->
    null;
normalize(V) ->
    list_to_binary(io_lib:format("~tp", [V])).

normalize_map_key(K) when is_binary(K) -> K;
normalize_map_key(K) when is_atom(K) -> list_to_binary(atom_to_list(K));
normalize_map_key(K) when is_list(K) -> list_to_binary(K);
normalize_map_key(K) when is_integer(K) -> list_to_binary(integer_to_list(K));
normalize_map_key(K) -> list_to_binary(io_lib:format("~tp", [K])).
