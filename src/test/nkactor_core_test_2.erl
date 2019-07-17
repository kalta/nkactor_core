%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc
-module(nkactor_core_test_2).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-import(nkactor_core_test_util, [
        req/1, kapi_req/1, api_watch/1, wait_api_event/2, api_watch_stop/1,
        http_get/1, http_post/2, http_put/2,
        http_delete/1, http_list/1, http_watch/1, wait_http_event/2, http_watch_stop/1,
        clean_events/0, yaml/1]).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("nkactor/include/nkactor.hrl").


%% ===================================================================
%% Public
%% ===================================================================

all_tests() ->
    ok = access_id_test(),
    nkactor_core_test_util:create_test_data(),
    ok = alarm_test(),
    ok = token_test(),
    ok = config_test(),
%%    ok = task_test(),
%%    ok = auto_activate_test(),
%%    ok = session_test(),
%%    ok = file_provider_test(),
%%    ok = file_test(),
%%    nkactor_core_test_util:delete_test_data(),
    ok.



access_id_test() ->
    req(#{verb=>delete, namespace=>"a.test.my_actors", resource=>accessids, name=>id1}),

    B1 = yaml(<<"
        data:
            key2: val2
        metadata:
            subtype: TestType
            namespace: a.test.my_actors
            annotations:
                ann1: value1
    ">>),
    {created, ID1} = kapi_req(#{verb=>create, resource=>accessids, name=>id1, body=>B1, params=>#{ttl=>2}}),
    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"AccessId">>,
        <<"data">> := #{
            <<"key2">> := <<"val2">>
        },
        <<"metadata">> := #{
            <<"subtype">> := <<"TestType">>,
            <<"namespace">> := <<"a.test.my_actors">>,
            <<"uid">> := ID1_UID,
            <<"name">> := <<"id1">>,
            <<"annotations">> := #{
                <<"ann1">> := <<"value1">>
            }
        }
    } = ID1,
    {ok, ID2} = kapi_req(#{verb=>get, namespace=>"a.test.my_actors", resource=>accessids, name=>id1, params=>#{activate=>false}}),
    {ok, #actor_id{pid=Pid1}} = nkactor:find(ID1_UID),
    true = is_pid(Pid1),

    timer:sleep(2100),
    {ok, #actor_id{pid=undefined}} = nkactor:find(ID1_UID),
    {ok, ID2} = kapi_req(#{verb=>get, namespace=>"a.test.my_actors", resource=>accessids, name=>id1, params=>#{activate=>false}}),
    {ok, #actor_id{pid=undefined}} = nkactor:find(ID1_UID),
    ok.


alarm_test() ->
    {ok, #{<<"items">>:=[_]}} = kapi_req(#{verb=>list, resource=>users, params=>#{deep=>true}}),
    {ok, #{<<"items">>:=[]}} =
        kapi_req(#{verb=>list, resource=>users, params=>#{fieldSelector=>"metadata.inAlarm:true"}}),
    P = "core:users:ut1.b.a.test.my_actors",
    {ok, []} = nkactor_srv:sync_op(P, get_alarms),
    {error, {field_missing, <<"class">>}} = nkactor_srv:sync_op(P, {set_alarm, #{
        <<"code">> => <<"code1">>,
        <<"message">> => <<"message1">>,
        <<"meta">> => #{<<"a">> => 1}
    }}),
    ok = nkactor_srv:sync_op(P, {set_alarm, #{
        class => class1,
        <<"code">> => <<"code1">>,
        <<"message">> => <<"message1">>,
        <<"meta">> => #{<<"a">> => 1}
    }}),
    ok = nkactor_srv:sync_op(P, {set_alarm, #{
        class => class2,
        <<"code">> => <<"code2">>,
        <<"message">> => <<"message2">>
    }}),
    ok = nkactor_srv:sync_op(P, {set_alarm, #{
        class => class1,
        <<"code">> => <<"code3">>,
        <<"message">> => <<"message1">>,
        <<"meta">> => #{<<"a">> => 2}
    }}),
    {ok, [
        #{
            class := <<"class1">>,
            code := <<"code3">>,
            message := <<"message1">>,
            last_time := <<"20", _/binary>>,
            meta := #{<<"a">> := 2}
        },
        #{
            class := <<"class2">>,
            code := <<"code2">>,
            last_time := <<"20", _/binary>>,
            message := <<"message2">>
        }
    ]} = nkactor_srv:sync_op(P, get_alarms),

    % Not yet saved
    {ok, #{<<"items">>:=[]}} = kapi_req(#{verb=>list, resource=>users, params=>#{deep=>true, fieldSelector=>"metadata.inAlarm:true"}}),
    {ok, #{save:=Time}} = nkactor_srv:get_timers(P),
    true = is_integer(Time),

    % Don't wait for automatic save
    ok = nkactor_srv:async_op(P, save),
    timer:sleep(50),
    {ok, #{<<"items">>:=[U1]}} =
        kapi_req(#{verb=>list, resource=>users, params=>#{deep=>true, fieldSelector=>"metadata.inAlarm:true"}}),
    #{
        <<"metadata">> := #{
            <<"inAlarm">> := true,
            <<"alarms">> := [
                #{
                    <<"class">> := <<"class1">>,
                    <<"code">> := <<"code3">>,
                    <<"message">> := <<"message1">>,
                    <<"lastTime">> := <<"20", _/binary>>,
                    <<"meta">> := #{<<"a">> := 2}
                },
                #{
                    <<"class">> := <<"class2">>,
                    <<"code">> := <<"code2">>,
                    <<"lastTime">> := <<"20", _/binary>>,
                    <<"message">> := <<"message2">>
                }
            ]
        }
    } = U1,

    % Clear
    ok = nkactor_srv:async_op(P, clear_all_alarms),
    timer:sleep(50),
    ok = nkactor_srv:async_op(P, save),
    timer:sleep(50),
    {ok, #{<<"items">>:=[]}} =
        kapi_req(#{verb=>list, resource=>users, params=>#{deep=>true, fieldSelector=>"metadata.inAlarm:true"}}),
    ok.



token_test() ->
    % Create a token
    B1 = yaml(<<"
        data:
            key2: val2
        metadata:
            subtype: TestType
            namespace: a.test.my_actors
            annotations:
                ann1: value1
    ">>),
    {error, #{<<"reason">>:=<<"ttl_missing">>}} = kapi_req(#{verb=>create, resource=>tokens, body=>B1}),

    {created, T1} = kapi_req(#{verb=>create, resource=>tokens, body=>B1, params=>#{ttl=>2}}),
    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"Token">>,
        <<"data">> := #{
            <<"key2">> := <<"val2">>
        },
        <<"metadata">> := #{
            <<"uid">> := _T1_UID,
            <<"name">> := T1_Name,
            <<"subtype">> := <<"TestType">>,
            <<"namespace">> := <<"a.test.my_actors">>,
            <<"expiresTime">> := ET,
            <<"annotations">> := #{
                <<"ann1">> := <<"value1">>
            }
        }
    } = T1,
    Now = nklib_date:epoch(msecs),
    {ok, ET2} = nklib_date:to_epoch(ET, msecs),
    true = (ET2-Now) > 1500 andalso (ET2-Now) < 2500,

    {ok, T1} = kapi_req(#{verb=>get, namespace=>"a.test.my_actors", resource=>tokens, name=>T1_Name}),

    % Wait for the token to expire
    timer:sleep(2100),
    {error, #{<<"reason">>:=<<"actor_not_found">>}} = kapi_req(#{verb=>get, resource=>tokens, name=>T1_Name}),
%%    nknamespace_api_events:wait_for_save(),
%%    {ok, #{<<"items">>:=Events, <<"metadata">>:=#{<<"total">>:=2}}} =
%%        kapi_req(#{verb=>list, namespace=>"a.test.my_actors", resource=>events, params=>#{
%%            fieldSelector=><<"involvedObject.uid:", T1_UID/binary>>
%%        }}),
%%    [
%%        #{<<"reason">>:=<<"ActorDeleted">>},
%%        #{<<"reason">>:=<<"ActorCreated">>, <<"body">>:=#{<<"actor">>:=T1}}
%%    ] = Events,

    % Test consume
    {created, T2} = kapi_req(#{verb=>create, resource=>tokens, body=>B1, params=>#{ttl=>2}}),
    #{<<"metadata">>:=#{<<"name">>:=T2_Name}} = T2,
    {ok, T2} = kapi_req(#{verb=>get, namespace=>"a.test.my_actors", resource=>tokens, name=>T2_Name}),
    {ok, T2} = kapi_req(
        #{verb=>get, namespace=>"a.test.my_actors", resource=>tokens, name=>T2_Name, params=>#{consume=>true}}),
    {error, #{<<"reason">>:=<<"actor_not_found">>}} = kapi_req(#{verb=>get, namespace=>"a.test.my_actors", resource=>tokens, name=>T2_Name}),

    % Test deletion on load
    {created, T3} = kapi_req(#{verb=>create, resource=>tokens, body=>B1, params=>#{ttl=>2}}),
    #{<<"metadata">>:=#{<<"name">>:=T3_Name}} = T3,
    Path = <<"core:tokens:", T3_Name/binary, ".a.test.my_actors">>,
    {true, Pid1} = nkactor:is_activated(Path),

    % We kill the actor, is its activated again on load
    exit(Pid1, kill),
    timer:sleep(50),
    false = nkactor:is_activated(Path),
    {ok, T3} = kapi_req(#{verb=>get, namespace=>"a.test.my_actors", resource=>tokens, name=>T3_Name}),
    timer:sleep(50),
    {true, Pid2} = nkactor:is_activated(Path),
    false = Pid1 == Pid2,

    % We kill again but when we activate it again is expired
    exit(Pid2, kill),
    timer:sleep(2100),
    Params = #{getTotals => true, fieldSelector=><<"metadata.name:", T3_Name/binary>>},
    % List don't activate, so it still shows the actor
    {ok, #{<<"metadata">>:=#{<<"total">>:=1}}} = kapi_req(#{verb=>list, namespace=>"a.test.my_actors", resource=>tokens, params=>Params}),
    {error, #{<<"reason">>:=<<"actor_not_found">>}} = kapi_req(#{verb=>get, namespace=>"a.test.my_actors", resource=>tokens, name=>T3_Name}),
    {ok, #{<<"metadata">>:=#{<<"total">>:=0}}} = kapi_req(#{verb=>list, namespace=>"a.test.my_actors", resource=>tokens, params=>Params}),
    ok.


config_test() ->
    kapi_req(#{verb=>delete, namespace=>"a.test.my_actors", resource=>configmaps, name=>c1}),

    B1 = yaml(<<"
        data:
            key2: val2
        metadata:
            subtype: TestType
            namespace: a.test.my_actors
            annotations:
                ann1: value1
    ">>),
    {created, C1} = kapi_req(#{verb=>create, resource=>configmaps, name=>c1, body=>B1, params=>#{ttl=>2}}),
    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"ConfigMap">>,
        <<"data">> := #{
            <<"key2">> := <<"val2">>
        },
        <<"metadata">> := #{
            <<"subtype">> := <<"TestType">>,
            <<"namespace">> := <<"a.test.my_actors">>,
            <<"uid">> := _C1_UID,
            <<"name">> := <<"c1">>,
            <<"annotations">> := #{
                <<"ann1">> := <<"value1">>
            }
        }
    } = C1,
    {ok, C1} = kapi_req(#{verb=>get, namespace=>"a.test.my_actors", resource=>configmaps, name=>c1, params=>#{activate=>false}}),
    ok.


task_test() ->
    {ok, _} = kapi_req(#{verb=>deletecollection, namespace=>"a.test.my_actors", resource=>tasks,
               params=>#{fieldSelector => <<"metadata.subtype:TestType">>}}),
    B1 = yaml(<<"
        spec:
            job:
                key1: val1
            maxSecs: 2
        metadata:
            subtype: TestType
            namespace: a.test.my_actors

    ">>),
    {created, T1} = kapi_req(#{verb=>create, resource=>tasks, body=>B1}),

    #{
        <<"apiVersion">> := <<"core/v1a1">>,
        <<"kind">> := <<"Task">>,
        <<"spec">> := #{
            <<"job">> := #{<<"key1">> := <<"val1">>},
            <<"maxSecs">> := 2,
            <<"maxTries">> := 3
        },
        <<"metadata">> := #{
            <<"subtype">> := <<"TestType">>,
            <<"namespace">> := <<"a.test.my_actors">>,
            <<"name">> := T1_Name
        },
        <<"status">> := #{
            <<"taskStatus">> := <<"start">>,
            <<"progress">> := 0,
            <<"lastTryStartTime">> := LT1,
            <<"tries">> := 1
        }
    } = T1,

    % Update state, we are on first try
    Url1 = "/namespaces/a.test.my_actors/tasks/"++binary_to_list(T1_Name),
    {400, #{<<"message">> := <<"Field 'taskStatus' is missing">>}} = http_put(Url1 ++ "/_state", #{a=>1}),
    {200, #{<<"reason">>:=<<"actor_updated">>}} = http_put(Url1 ++ "/_state", #{taskStatus=>progress, progress=>50}),

    {200, T2} = http_get(Url1),
    #{
        <<"status">> := #{
            <<"taskStatus">> := <<"progress">>,
            <<"progress">> := 50,
            <<"lastTryStartTime">> := LT1,
            <<"tries">> := 1
        }
    } = T2,

    % Signal an error. The task will stop
    {200, #{<<"reason">>:=<<"actor_updated">>}} = http_put(Url1 ++ "/_state", #{taskStatus=>error, errorMsg=>err1}),
    timer:sleep(100),

    % We reactivate the task (second try)  and kill again
    {200, T3} = http_get(Url1),
    #{
        <<"status">> := #{
            <<"taskStatus">> := <<"start">>,
            <<"progress">> := 0,
            <<"lastTryStartTime">> := LT3,
            <<"tries">> := 2
        }
    } = T3,
    true = LT3 > LT1,
    {200, #{<<"reason">>:=<<"actor_updated">>}} = http_put(Url1 ++ "/_state", #{taskStatus=>error, errorMsg=>err1}),
    timer:sleep(100),

    % We reactivate the task (third try) and kill again, it will reach max tries
    {200, T4} = http_get(Url1),
    #{
        <<"status">> := #{
            <<"taskStatus">> := <<"start">>,
            <<"progress">> := 0,
            <<"lastTryStartTime">> := LT4,
            <<"tries">> := 3
        }
    } = T4,
    true = LT4 > LT3,
    {200, #{<<"reason">>:=<<"actor_updated">>}} = http_put(Url1 ++ "/_state", #{taskStatus=>error, errorMsg=>err1}),
    timer:sleep(100),

    % The task will stop again. If we try to restart it, it fails and it is deleted
    {422, #{<<"reason">>:=<<"task_max_tries_reached">>}} = http_get(Url1),
    {404, _} = http_get(Url1),

%%    % Get events
%%    nknamespace_api_events:wait_for_save(),
%%    {ok, #{<<"items">>:=Events1}} = kapi_req(#{verb=>list, namespace=>"a.test.my_actors", resource=>events, params=>#{
%%        fieldSelector=><<"involvedObject.kind:Task,involvedObject.name:", T1_Name/binary>>}}),
%%    [
%%        #{
%%            <<"reason">> := <<"ActorDeleted">>,
%%            <<"involvedObject">> := #{
%%                <<"subtype">> := <<"TestType">>
%%            }
%%        },
%%        #{
%%
%%            <<"reason">> := <<"TaskFaillure">>,
%%            <<"involvedObject">> := #{
%%                <<"subtype">> := <<"TestType">>
%%            },
%%            <<"message">> := <<"task_max_tries_reached">>,
%%            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 3}
%%        },
%%        #{
%%            <<"reason">> := <<"TaskError">>,
%%            <<"message">> := <<"err1">>,
%%            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 3}
%%        },
%%        #{
%%            <<"reason">> := <<"TaskStart">>,
%%            <<"message">> := <<>>,
%%            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 3}
%%        },
%%        #{
%%            <<"reason">> := <<"TaskError">>,
%%            <<"message">> := <<"err1">>,
%%            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 2}
%%        },
%%        #{
%%            <<"reason">> := <<"TaskStart">>,
%%            <<"message">> := <<>>,
%%            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 2}
%%        },
%%        #{
%%            <<"reason">> := <<"TaskError">>,
%%            <<"message">> := <<"err1">>,
%%            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 1}
%%        },
%%        #{
%%            <<"reason">> := <<"TaskStart">>,
%%            <<"message">> := <<>>,
%%            <<"body">> := #{
%%                <<"maxTries">> := 3,
%%                <<"tries">> := 1,
%%                <<"job">> := #{<<"key1">> := <<"val1">>},
%%                <<"maxSecs">> := 2
%%            }
%%        },
%%        #{
%%            <<"reason">> := <<"ActorCreated">>,
%%            <<"body">> := #{<<"actor">> := _}
%%        }
%%    ] = Events1,

    % We create another task and wait for expire
    {created, T5} = kapi_req(#{verb=>create, resource=>tasks, body=>B1}),
    #{
        <<"metadata">> := #{
            <<"name">> := T5_Name
        },
        <<"status">> := #{
            <<"taskStatus">> := <<"start">>,
            <<"tries">> := 1
        }
    } = T5,
    % Wait for expire, the task will be deleted
    timer:sleep(2100),
    {404, _} = http_get(Url1),


%%    nknamespace_api_events:wait_for_save(),
%%    {ok, #{<<"items">>:=Events2}} = kapi_req(#{verb=>list, namespace=>"a.test.my_actors", resource=>events, params=>#{
%%        fieldSelector=><<"involvedObject.kind:Task,involvedObject.name:", T5_Name/binary>>}}),
%%    [
%%        #{
%%            <<"reason">> := <<"ActorDeleted">>
%%        },
%%        #{
%%
%%            <<"reason">> := <<"TaskFaillure">>,
%%            <<"message">> := <<"task_max_time_reached">>,
%%            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 1}
%%        },
%%        #{
%%            <<"reason">> := <<"TaskStart">>,
%%            <<"body">> := #{
%%                <<"maxTries">> := 3,
%%                <<"tries">> := 1,
%%                <<"job">> := #{<<"key1">> := <<"val1">>},
%%                <<"maxSecs">> := 2
%%            }
%%        },
%%        #{
%%            <<"reason">> := <<"ActorCreated">>
%%        }
%%    ] = Events2,

    % Create another instance and do a successful stop
    {created, T6} = kapi_req(#{verb=>create, resource=>tasks, body=>B1}),
    #{<<"metadata">> := #{<<"name">> := T6_Name}} = T6,
    ActorPath = <<"core:tasks:", T6_Name/binary, ".a.test.my_actors">>,
    ok = nkactor_srv:sync_op(ActorPath, {update_state, #{task_status=>success}}),
    timer:sleep(150),
    {error, actor_not_found} = nkactor:get_actor(ActorPath),
%%    nknamespace_api_events:wait_for_save(),
%%    {ok, #{<<"items">>:=Events3}} = kapi_req(#{verb=>list, namespace=>"a.test.my_actors", resource=>events, params=>#{
%%        fieldSelector=><<"involvedObject.kind:Task,involvedObject.subtype:TestType,involvedObject.name:", T6_Name/binary>>}}),
%%    %%    Ev2 = [maps:with([<<"reason">>, <<"message">>, <<"body">>], E) ||  E <- Events3],
%%    %%    io:format("NKLOG EV2 ~s\n", [nklib_json:encode_pretty(Ev2)]),
%%    [
%%        #{
%%            <<"reason">> := <<"ActorDeleted">>
%%        },
%%        #{
%%            <<"reason">> := <<"TaskSuccess">>,
%%            <<"body">> := #{<<"maxTries">> := 3,<<"tries">> := 1}
%%        },
%%        #{
%%            <<"reason">> := <<"TaskStart">>,
%%            <<"body">> := #{
%%                <<"maxTries">> := 3,
%%                <<"tries">> := 1,
%%                <<"job">> := #{<<"key1">> := <<"val1">>},
%%                <<"maxSecs">> := 2
%%            }
%%        },
%%        #{
%%            <<"reason">> := <<"ActorCreated">>
%%        }
%%    ] = Events3,
    ok.
%%
%%
%%auto_activate_test() ->
%%    %% See nknamespace_lib:launch_auto_activated/1
%%
%%    kapi_req(#{verb=>deletecollection, namespace=>"a.test.my_actors", resource=>tasks, params=>#{
%%        fieldSelector => <<"metadata.subtype:TestType">>}}),
%%
%%    Y1 = yaml(<<"
%%        spec:
%%            maxSecs: 2
%%        metadata:
%%            subtype: TestType
%%            namespace: a.test.my_actors
%%    ">>),
%%
%%    % Create 10 tasks expiring in 2 secs
%%    Pids1 = lists:map(
%%        fun(Pos) ->
%%            Name = nklib_util:to_binary(Pos),
%%            {created, T} = kapi_req(#{verb=>create, resource=>tasks, name=>Name, body=>Y1}),
%%            #{<<"metadata">>:=#{<<"name">>:=Name}} = T,
%%            P = <<"/a.test.my_actors/core/tasks/", Name/binary>>,
%%            {ok, #actor_id{pid=Pid}, _} = nkservice_actor:find(P),
%%            true = is_pid(Pid),
%%            Pid
%%        end,
%%        lists:seq(1, 10)),
%%
%%    % They are on DB
%%    {ok, #{<<"metadata">>:=#{<<"total">>:=10}}} =
%%        kapi_req(#{verb=>list, namespace=>"a.test.my_actors", resource=>tasks}),
%%
%%    % They are already activated
%%    [] = nkactor_api_util:launch_auto_activated(?ROOT_SRV),
%%
%%    % We kill them
%%    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids1),
%%    timer:sleep(50),
%%
%%    % They are reactivated by script
%%    % (you can experiment with values in size, for example 5)
%%    ActivatedIds = nkactor_api_util:launch_auto_activated(?ROOT_SRV),
%%    10 = length(ActivatedIds),
%%    [] = nkactor_api_util:launch_auto_activated(?ROOT_SRV),
%%    Pids2 = lists:map(
%%        fun(ActorId) ->
%%            {true, #actor_id{pid=Pid}} = nkactor:is_activated(ActorId),
%%            Pid
%%        end,
%%        ActivatedIds),
%%
%%    % We kill them again, and wait for expiration while unloaded
%%    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids2),
%%    timer:sleep(2100),
%%
%%    % They expired, but yet on db. When we reactivate them, they are deleted
%%    {ok, #{<<"metadata">>:=#{<<"total">>:=10}}} =
%%        kapi_req(#{verb=>list, namespace=>"a.test.my_actors", resource=>tasks}),
%%    lager:error("NKLOG START"),
%%    ActivatedIds = nkactor_api_util:launch_auto_activated(?ROOT_SRV),
%%    timer:sleep(500),
%%    {ok, #{<<"metadata">>:=#{<<"total">>:=0}}} =
%%        kapi_req(#{verb=>list, namespace=>"a.test.my_actors", resource=>tasks}),
%%    ok.
%%
%%
%%
%%
%%session_test() ->
%%    kapi_req(#{verb=>deletecollection, namespace=>"a.test.my_actors", resource=>sessions, params=>#{
%%        fieldSelector => <<"metadata.subtype:TestType">>}}),
%%
%%    Y1 = yaml(<<"
%%        spec:
%%            ttlSecs: 2
%%        data:
%%            a: 1
%%        metadata:
%%            name: s1
%%            subtype: TestType
%%            namespace: a.test.my_actors
%%    ">>),
%%
%%    {created, S1} = kapi_req(#{verb=>create, resource=>sessions, body=>Y1}),
%%    #{
%%        <<"apiVersion">> := <<"core/v1a1">>,
%%        <<"kind">> := <<"Session">>,
%%        <<"data">> := #{
%%            <<"a">> := 1
%%        },
%%        <<"metadata">> := #{
%%            <<"subtype">> := <<"TestType">>,
%%            <<"namespace">> := <<"a.test.my_actors">>,
%%            <<"uid">> := _S1_UID,
%%            <<"name">> := <<"s1">>
%%        },
%%        <<"status">> := #{<<"isActivated">>:=true}
%%    } = S1,
%%
%%    timer:sleep(1000),
%%    P = "/a.test.my_actors/core/sessions/s1",
%%    {ok, {expires, Time1}} = nkactor_srv:sync_op(P, get_unload_policy),
%%    true = (Time1 - nklib_date:epoch(msecs)) < 1000,
%%
%%    lager:error("NKLOG LAUNCH"),
%%    {ok, #{<<"reason">>:= <<"actor_updated">>}} =
%%        kapi_req(#{verb=>get, namespace=>"a.test.my_actors", resource=>"sessions", name=>s1, subresource=><<"_rpc/refresh">>}),
%%
%%    {ok, {expires, Time2}} = nkactor_srv:sync_op(P, get_unload_policy),
%%    true = (Time2 - nklib_date:epoch(msecs)) > 1500,
%%
%%    timer:sleep(2100),
%%    {error, #{<<"reason">> := <<"actor_not_found">>}} =
%%        kapi_req(#{verb=>get, namespace=>"a.test.my_actors", resource=>"sessions", name=>s1, subresource=><<"_rpc/refresh">>}),
%%    ok.
%%
%%
%%file_provider_test() ->
%%    kapi_req(#{verb=>deletecollection, namespace=>"a.test.my_actors", resource=>files}),
%%    kapi_req(#{verb=>deletecollection, namespace=>"a.test.my_actors", resource=>fileproviders}),
%%
%%    Y1 = yaml(<<"
%%        metadata:
%%            name: fs1
%%            namespace: a.test.my_actors
%%    ">>),
%%    {error, #{<<"message">>:=<<"Missing field: 'spec'">>}} = kapi_req(#{verb=>create, resource=>fileproviders, body=>Y1}),
%%
%%    Y1b = yaml(<<"
%%        spec:
%%            storageClass: unknown
%%        metadata:
%%            name: fs1
%%            namespace: a.test.my_actors
%%    ">>),
%%    {error, #{<<"message">>:=<<"Field 'spec.storageClass' is invalid">>}} = kapi_req(#{verb=>create, resource=>fileproviders, body=>Y1b}),
%%
%%
%%    Y2 = yaml(<<"
%%        spec:
%%            storageClass: filesystem
%%            encryptionAlgo: aes_cfb128
%%        metadata:
%%            name: fs1
%%            namespace: a.test.my_actors
%%    ">>),
%%    {error, #{<<"message">>:=<<"Missing field: 'spec.filesystemConfig'">>}} = kapi_req(#{verb=>create, resource=>fileproviders, body=>Y2}),
%%
%%
%%    YFP1 = yaml(<<"
%%        kind: FileProvider
%%        spec:
%%            storageClass: filesystem
%%            maxSize: 3
%%            encryptionAlgo: aes_cfb128
%%            hashAlgo: sha256
%%            filesystemConfig:
%%                filePath: '/tmp'
%%        metadata:
%%            name: fs1
%%            namespace: a.test.my_actors
%%    ">>),
%%
%%    {created, FP1} = kapi_req(#{verb=>create, body=>YFP1}),
%%    #{
%%        <<"apiVersion">> := <<"core/v1a1">>,
%%        <<"kind">> := <<"FileProvider">>,
%%        spec := #{
%%            <<"storageClass">> := <<"filesystem">>,
%%            <<"encryptionAlgo">> := <<"aes_cfb128">>,
%%            <<"hashAlgo">> := <<"sha256">>,
%%            <<"filesystemConfig">> := #{
%%                <<"filePath">> := <<"/tmp">>
%%            }
%%        },
%%        <<"metadata">> := #{
%%            <<"namespace">> := <<"a.test.my_actors">>,
%%            <<"uid">> := FP1_UID,
%%            <<"name">> := <<"fs1">>
%%        },
%%        <<"status">> := #{<<"isActivated">>:=true}
%%    } = FP1,
%%    {ok, _, #{
%%        id := FP1_UID,
%%        storageClass := <<"filesystem">>,
%%        encryptionAlgo := aes_cfb128,
%%        hashAlgo := sha256,
%%        maxSize := 3,
%%        filesystemConfig := #{filePath := <<"/tmp">>}
%%    }} = nknamespace_file_provider_actor:op_get_spec(?ROOT_SRV, "/a.test.my_actors/core/fileproviders/fs1"),
%%
%%    SFP2 = <<"
%%        kind: FileProvider
%%        spec:
%%            storageClass: s3
%%            encryptionAlgo: aes_cfb128
%%            s3Config:
%%                scheme: http
%%                host: localhost
%%                port: 9000
%%                key: '5UBED0Q9FB7MFZ5EWIO'
%%                secret: 'CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI'
%%                bucket: bucket1
%%        metadata:
%%            name: fs2
%%            namespace: a.test.my_actors
%%    ">>,
%%
%%    {created, _FP2} = kapi_req(#{verb=>create, body=>yaml(SFP2)}),
%%
%%    SFP3 = re:replace(SFP2, <<"bucket: bucket1">>, <<"bucket: bucket2">>, [{return, binary}]),
%%    {error, #{<<"message">>:=<<"Tried to update immutable field: 'spec.s3Config.bucket'">>}} = kapi_req(#{verb=>update, body=>yaml(SFP3)}),
%%
%%    SFP4 = re:replace(SFP2, <<"encryptionAlgo:">>, <<"#encryptionAlgo:">>, [{return, binary}]),
%%    {error, #{<<"message">>:=<<"Tried to update immutable field: 'spec.encryptionAlgo'">>}} = kapi_req(#{verb=>update, body=>yaml(SFP4)}),
%%
%%    SFP5 = re:replace(SFP2, <<"host: localhost">>, <<"host: 127.0.0.1">>, [{return, binary}]),
%%    {ok, #{spec:=#{<<"s3Config">>:=#{<<"host">>:=<<"127.0.0.1">>}}}} = kapi_req(#{verb=>update, body=>yaml(SFP5)}),
%%    ok.
%%
%%
%%file_test() ->
%%    kapi_req(#{verb=>deletecollection, namespace=>"a.test.my_actors", resource=>files}),
%%    {ok, #{<<"metadata">>:=#{<<"uid">>:=FS1_UID}}} =
%%        kapi_req(#{verb=>get, namespace=>"a.test.my_actors", resource=>"fileproviders", name=>fs1}),
%%
%%    % Create a file with bodyBase64
%%    Body = base64:encode(<<"123">>),
%%    Y1 = yaml(<<"
%%        kind: File
%%        spec:
%%            contentType: type1
%%            bodyBase64: '", Body/binary, "'
%%            provider: /apis/core/v1a1/namespaces/a.test.my_actors/fileproviders/fs1
%%        metadata:
%%            name: f1
%%    ">>),
%%    {created, F1} = kapi_req(#{verb=>create, namespace=>"a.test.my_actors", body=>Y1}),
%%    Hash = base64:encode(crypto:hash(sha256, <<"123">>)),
%%    #{
%%        <<"apiVersion">> := <<"core/v1a1">>,
%%        <<"kind">> := <<"File">>,
%%        spec := #{
%%            <<"contentType">> := <<"type1">>,
%%            <<"externalId">> := F1_UID,
%%            <<"provider">> := <<"/apis/core/v1a1/namespaces/a.test.my_actors/fileproviders/fs1">>,
%%            <<"size">> := 3,
%%            <<"password">> := _,
%%            <<"hash">> := Hash
%%        } = Spec1,
%%        <<"metadata">> := #{
%%            <<"uid">> := F1_UID,
%%            <<"name">> := <<"f1">>,
%%            <<"links">> := #{
%%                FS1_UID := <<"io.netc.core.file-provider">>
%%            }
%%        } = Meta1,
%%        <<"status">> := #{<<"isActivated">> := true}
%%    } = F1,
%%    false = maps:is_key(<<"bodyBase64">>, Spec1),
%%
%%    % Check it is on disk
%%    {ok, B2} = file:read_file(<<"/tmp/", F1_UID/binary>>),
%%    3 = byte_size(B2),
%%    true = B2 /= <<"123">>,
%%
%%    % Cannot remove provider
%%    {error, #{<<"reason">>:=<<"actor_has_linked_actors">>}} =
%%        kapi_req(#{verb=>delete, namespace=>"a.test.my_actors", resource=>"fileproviders", name=>fs1}),
%%
%%    % Get the body direct
%%    {ok, <<"type1">>, <<"123">>} = nknamespace_file_actor:op_get_body(?ROOT_SRV, F1_UID),
%%
%%    % Get the object and the body inline
%%    {200, F1} = http_get("/namespaces/a.test.my_actors/files/f1"),
%%    {200, F2} = http_get("/namespaces/a.test.my_actors/files/f1?getBodyInline=true"),
%%    #{spec:=#{<<"bodyBase64">>:=Body}} = F2,
%%
%%    % Get a direct download
%%    {ok, {{_, 200, _}, Hds, "123"}} = nkactor_core_test_util:httpc("/namespaces/a.test.my_actors/files/f1/_download"),     "type1" = nklib_util:get_value("content-type", Hds),
%%
%%
%%    % Cannot update file
%%    Y2 = maps:remove(<<"status">>, F1#{
%%        spec := Spec1#{
%%            <<"contentType">> := <<"type2">>
%%        }
%%    }),
%%    {error, #{ <<"message">>:=<<"Tried to update immutable field: 'spec.contentType'">>}} =
%%        kapi_req(#{verb=>update, namespace=>"a.test.my_actors", name=>f1, body=>Y2}),
%%
%%    % But can add annotations, etc.
%%    Y3 = maps:remove(<<"status">>, F1#{
%%        <<"metadata">> := Meta1#{
%%            <<"annotations">> => #{
%%                <<"ann1">> => <<"v1">>
%%            }
%%        }
%%    }),
%%    {ok, F3} = kapi_req(#{verb=>update, body=>Y3}),
%%    #{<<"metadata">> := #{<<"annotations">> := #{<<"ann1">>:=<<"v1">>}}} = F3,
%%
%%    % Filesystem uses standard downloads
%%    {200, #{<<"url">> := Url}} = http_get("/namespaces/a.test.my_actors/files/f1/_rpc/downloadLink"),
%%    <<"_download">> = lists:last(binary:split(Url, <<"/">>, [global])),
%%
%%    % Send direct to _upload
%%    {ok, {{_, 400, _}, _Hds, Body4}} = nkactor_core_test_util:httpc(post, "/namespaces/a.test.my_actors/files/f1/_upload", "ct2", <<"321">>),
%%    #{ <<"message">> := <<"Missing field: 'provider'">>} = nklib_json:decode(Body4),
%%    {ok, {{_, 201, _}, _, Body5}} = nkactor_core_test_util:httpc(
%%        post,
%%        "/namespaces/a.test.my_actors/files/_upload?provider=/apis/core/v1a1/namespaces/a.test.my_actors/fileproviders/fs1",
%%        "ct2",
%%        <<"321">>),
%%    #{
%%        <<"apiVersion">> := <<"core/v1a1">>,
%%        <<"kind">> := <<"File">>,
%%        <<"metadata">> := #{
%%            <<"namespace">> := <<"a.test.my_actors">>,
%%            <<"name">> := _,
%%            <<"links">> := #{
%%                FS1_UID := <<"io.netc.core.file-provider">>
%%            }
%%        },
%%        spec := #{
%%            <<"contentType">> := <<"ct2">>,
%%            <<"externalId">> := <<"files-", _/binary>>,
%%            <<"hash">> := Hash2,
%%            <<"password">> := _,
%%            <<"provider">> := <<"/apis/core/v1a1/namespaces/a.test.my_actors/fileproviders/fs1">>,
%%            <<"size">> := 3
%%        }
%%    } = nklib_json:decode(Body5),
%%    Hash2 = base64:encode(crypto:hash(sha256, <<"321">>)),
%%
%%    % Direct to _upload, but through provider, first one is too large
%%    {ok, {{_, 400, _}, _, Body6}} =
%%        nkactor_core_test_util:httpc(
%%            post,
%%            "/namespaces/a.test.my_actors/fileproviders/fs1/files/_upload",
%%            "ct3",
%%            <<"4321">>),
%%    #{<<"reason">> := <<"file_too_large">>} = nklib_json:decode(Body6),
%%
%%    % Direct to _upload, but through provider
%%    {ok, {{_, 201, _}, _, Body7}} = nkactor_core_test_util:httpc(
%%        post,
%%        "/namespaces/a.test.my_actors/fileproviders/fs1/files/_upload",
%%        "ct3",
%%        <<"321">>),
%%    #{spec := #{<<"contentType">>:=<<"ct3">>, <<"hash">>:=Hash2}} = nklib_json:decode(Body7),
%%    ok.
%%
%%
