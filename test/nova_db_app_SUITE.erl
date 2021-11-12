-module(nova_db_app_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(BASEPATH, <<"http://localhost:8080/">>).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(_Config) ->
    Path = [?BASEPATH, <<"message">>],
    Message = json:encode(#{<<"payload">> => <<"This is an awesome application!">>}, [maps, binary]),
    #{status := {201, _}, body := RespBody} = shttpc:post(Path, Message, opts(json_post)),
    #{<<"id">> := Id} = json:decode(RespBody, [maps]),
    [{id, Id}].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    Id = proplists:get_value(id, Config),
    Path = [?BASEPATH, <<"message/">>, Id],
    #{status := {200, _}} = shttpc:delete(Path, opts()),
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     get_all_messages,
     get_message,
     update_message].
%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------

get_all_messages(_) ->
    Path = [?BASEPATH, <<"message">>],
    #{status := {200, _}, body := RespBody} = shttpc:get(Path, opts(json_get)),
    1 = length(json:decode(RespBody, [maps])).

get_message(Config) ->
    Id = proplists:get_value(id, Config),
    Path = [?BASEPATH, <<"message/">>, Id],
    #{status := {200, _}, body := RespBody} = shttpc:get(Path, opts(json_get)),
    #{<<"id">> := Id} = json:decode(RespBody, [maps]).

update_message(Config) ->
    Id = proplists:get_value(id, Config),
    Path = [?BASEPATH, <<"message/">>, Id],
    Json = json:encode(#{<<"payload">> => <<"Did you know this is awesome!">>}, [maps, binary]),
    #{status := {200, _}, body := RespBody} = shttpc:put(Path, Json, opts(json_post)),
    #{<<"id">> := Id,
      <<"payload">> := <<"Did you know this is awesome!">>} = json:decode(RespBody, [maps]).

opts() ->
    opts(undefined).
opts(undefined) ->
    #{headers => #{}, close => true};
opts(form) ->
    #{headers => #{'Content-Type' => <<"application/x-www-form-urlencoded">>}, close => true};
opts(json_get) ->
    #{headers => #{'Accept' => <<"application/json">>}, close => true};
opts(json_post) ->
    #{headers => #{'Content-Type' => <<"application/json">>}, close => true}.

websocket(Path, _Token) ->
    {ok, ConnPid} = gun:open("localhost", 8080, #{transport => tcp}),
    {ok, _Protocol} = gun:await_up(ConnPid),
    io:format("ConnPid: ~p", [ConnPid]),
    gun:ws_upgrade(ConnPid, Path, []),

    receive
        {gun_upgrade, ConnPid, _StreamRef, _Protocols, _Headers} ->
            ConnPid;
        {gun_response, ConnPid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, _ConnPid, _StreamRef, Reason} ->
            exit({ws_upgrade_failed, Reason});
        Err ->
            io:format("WS unexpectedly received ~p", [Err])

            %% More clauses here as needed.
    after 2000 ->
            exit(timeout)
    end.

encode(Json) ->
    json:encode(Json, [maps, binary]).