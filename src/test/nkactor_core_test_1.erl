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
-module(nkactor_core_test_1).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-import(nkactor_core_test_util, [
        req/1, api_watch/1, wait_api_event/2, api_watch_stop/1,
        http_get/1, http_post/2, http_put/2,
        http_delete/1, http_list/1, http_search/2, http_search_delete/2,
        http_watch/1, wait_http_event/2, http_watch_stop/1,
        clean_events/0, yaml/1, get_linked_uids/2]).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("nkactor/include/nkactor.hrl").


%% ===================================================================
%% Public
%% ===================================================================

t1() ->
    httpc:request("http://127.0.0.1:9001/apis/core/v1a1/domains?fts=dom%C3%A1in").
%%
%%all_tests() ->
%%    ok = basic_test(),
%%    ok = activation_test(),
%%    nkactor_core_test_util:delete_test_data(),
%%    ok = subdomains_test(),
%%    nkactor_core_test_util:delete_test_data(),
%%    ok = loading_test(),
%%    ok = disable_test(),
%%    nkactor_core_test_util:create_test_data(),
%%    ok = list_test_1(),
%%    nkactor_core_test_util:create_test_data(),
%%    ok = list_test_2(),
%%    nkactor_core_test_util:create_test_data(),
%%    ok = search_test(),
%%    nkactor_core_test_util:create_test_data(),
%%    ok = contact_test(),
%%    nkactor_core_test_util:delete_test_data(),
%%    ok = event_test(),
%%    nkactor_core_test_util:delete_test_data(),
%%    ok.


basic_test() ->
    req(#{verb=>delete, resource=>users, name=>"ut1"}),

    Path1 = <<"core:users:ut1.test.my_actors">>,
    {error, actor_not_found} = nkactor:get_actor(Path1),

    {error, actor_not_found} = req(#{verb=>get, resource=>users, name=>ut1}),
    {error, verb_not_allowed} = req(#{verb=>get1, resource=>users, name=>ut1}),
    {error, resource_invalid} = req(#{verb=>get, resource=>users2, name=>ut1}),
%%
    % Create an user
    U1 = #{
        group => core,
        name => ut1,
        data => #{
            spec => #{
                password => pass1
            }
        }
    },
    {created, Actor1} = req(#{verb=>create, resource=>users, name=>"ut1", body=>U1}),
    #{
        group := <<"core">>,
        resource := <<"users">>,
        name := <<"ut1">>,
        namespace := <<"test.my_actors">>,
        uid := UID,
        data := #{
            spec := #{
                password := <<>>
            }
        },
        metadata := #{
            creation_time := _Time1,
            generation := 0,
            hash := _Vsn1,
            update_time := _Time1
        }
    } = Actor1,

    % Check pass
    {error, {parameter_missing, <<"password">>}} = req(#{verb=>get, resource=>users, name=>ut1, subresource=>"_rpc/checkpass"}),
    {status, password_valid} = req(#{verb=>get, resource=>users, name=>ut1, subresource=>"_rpc/checkpass", params=>#{password=>"pass1"}}),
    {status, password_invalid} = req(#{verb=>get, resource=>users, name=>ut1, subresource=>"_rpc/checkpass", params=>#{password=>"pass2"}}),

    % We cannot create it again
    {error, uniqueness_violation} = req(#{verb=>create, resource=>users, name=>"ut1", body=>U1}),


    % The actor is loaded
    {true, my_actors, #actor_id{name = <<"ut1">>, pid=Pid1}= ActorId1} = nkactor_namespace:find_actor(UID),
    true = is_pid(Pid1),
    {ok, ActorId1} = nkactor:find(ActorId1),
    {ok, ActorId1} = nkactor:find(UID),
    {ok, Path1} = nkactor:get_path(UID),
    {ok, ActorId1} = nkactor:find(Path1),
    {ok, ActorId1} = nkactor:find(<<"users:ut1.test.my_actors">>),
    {error, actor_not_found} = nkactor:find(<<"users:ut2.test.my_actors">>),
    {error, actor_not_found} = nkactor:find(<<"users2:ut1.test.my_actors">>),
    {ok, ActorId1} = nkactor:find(<<"ut1.test.my_actors">>),
    {error, actor_not_found} = nkactor:find(<<"ut2.test.my_actors">>),
    {ok, Actor1} = nkactor:get_actor(Path1),
    {ok, Actor1} = req(#{verb=>get, uid=>UID}),
    {ok, Actor1} = req(#{verb=>get, resource=>"users", name=>"ut1"}),


    % Let's work with the actor while unloaded
    ok = nkactor:stop(Pid1),
    timer:sleep(100),
    false = nkactor_namespace:find_actor(UID),
    false = is_process_alive(Pid1),
    ActorId1b = ActorId1#actor_id{pid=undefined},
    {ok, ActorId1b} = nkactor:find(ActorId1),
    {ok, ActorId1b} = nkactor:find(UID),
    {ok, Actor2} = nkactor:get_actor(UID, #{activate=>false}),
    #{data:=#{spec:=#{password:=_}}=Data} = Actor2,
    Actor2 = Actor1#{data:=Data},
    {ok, Actor2} = nkactor:get_actor(Path1, #{activate=>false}),
    {ok, Actor2} = req(#{verb=>get, uid=>UID, params=>#{activate=>false}}),
    {ok, Actor2} = req(#{verb=>get, resource=>users, name=>ut1, params=>#{activate=>false}}),
    false = nkactor_namespace:find_actor(UID),

    % Load with TTL
    {ok, Actor1} = req(#{verb=>get, resource=>users, name=>ut1, params=>#{ttl=>500}}),
    timer:sleep(50),
    {true, my_actors, #actor_id{pid=Pid2}} = nkactor_namespace:find_actor(UID),
    true = Pid1 /= Pid2,
    timer:sleep(600),
    false = nkactor_namespace:find_actor(UID),

    % Delete the user
    {error, actor_not_found} = req(#{verb=>delete, resource=>users, name=>"utest1"}),
    {status, actor_deleted} = req(#{verb=>delete, resource=>users, name=>"ut1"}),
    ok.


activation_test() ->
    U1 = #{
        group => core,
        resource => users,
        name => ut1,
        namespace => <<"test.my_actors">>,
        data => #{
            spec => #{password => pass1}
        }
    },

    % Invalid fields
    {error, {field_invalid, <<"name">>}} =
        req(#{verb=>create, resource=>users, name=>"utest1", body=>U1}),
    {error,{namespace_not_found,<<"domain1">>}} =
        req(#{verb=>create, resource=>users, namespace=>domain1, name=>"ut1", body=>U1}),
    {error, {field_invalid, <<"group">>}} =
        req(#{verb=>create, resource=>users, name=>"utest1", body=>U1#{group=>core2}}),
    {error, {field_invalid, <<"resource">>}} =
        req(#{verb=>create, resource=>users, name=>"utest1", body=>U1#{resource=>users2}}),

    req(#{verb=>delete, resource=>users, name=>"ut1"}),

    % Create object without activation
    {ok, Actor1} = nkactor:create(U1, #{activate=>false, get_actor=>true}),
    #{
        group := <<"core">>,
        resource := <<"users">>,
        name := <<"ut1">>,
        namespace := <<"test.my_actors">>,
        data := #{
            spec := #{password := _}
        },
        uid := UID1,
        metadata := #{
            vsn := <<"0">>,
            creation_time := Time,
            generation := 0,
            hash := _Vsn2,
            update_time := Time
        }
    } = Actor1,
    false = nkactor_namespace:find_actor(UID1),
    % Delete not-activated object
    ok = nkactor:delete(<<"core:users:ut1.test.my_actors">>),

    % Create with TTL
    {ok, #actor_id{uid=UID2}} = nkactor:create(U1, #{ttl=>500}),
    {true, my_actors, _} = nkactor_namespace:find_actor(UID2),

    timer:sleep(600),
    false = nkactor_namespace:find_actor(UID2),
    ok = nkactor:delete("ut1.test.my_actors").



namespaces_test() ->
    % Delete all and stop namespaces
    nkactor:search_delete(my_actors, #{namespace=>my_actors, deep=>true, do_delete=>true}),
    nkactor_namespace:stop_namespace(<<"c.b.a.test.my_actors">>, normal),
    nkactor_namespace:stop_namespace(<<"a.test.my_actors">>, normal),
    timer:sleep(50),

    [{<<"my_actors">>, my_actors, _}] = nkactor_master:get_all_namespaces(<<>>),
    [{<<"my_actors">>, my_actors, _}] = nkactor_master:get_all_namespaces("my_actors"),
    [] = nkactor_master:get_all_namespaces("my_actors2"),

    % Create a config at "c.b.a.test.my_actors"
    {created, _} = req(#{verb=>create, resource=>configmaps, name=>config_c, namespace=>'c.b.a.test.my_actors'}),
    #{<<"core:configmaps">>:=1} = nkactor_namespace:get_counters(<<>>),
    true = map_size(nkactor_namespace:get_counters("my_actors2")) == 0,
    #{<<"core:configmaps">>:=1} = nkactor_namespace:get_counters("my_actors"),
    #{<<"core:configmaps">>:=1} = nkactor_namespace:get_counters("a.test.my_actors"),
    #{<<"core:configmaps">>:=1} = nkactor_namespace:get_counters("b.a.test.my_actors"),
    #{<<"core:configmaps">>:=1} = nkactor_namespace:get_counters("c.b.a.test.my_actors"),
    true = map_size(nkactor_namespace:get_counters("d.c.b.a.test.my_actors")) == 0,

    {created, _} = req(#{verb=>create, resource=>configmaps, name=>config_a, namespace=>'a.test.my_actors'}),
    #{<<"core:configmaps">>:=2} = nkactor_namespace:get_counters(<<>>),
    #{<<"core:configmaps">>:=2} = nkactor_namespace:get_counters("my_actors"),
    #{<<"core:configmaps">>:=2} = nkactor_namespace:get_counters("a.test.my_actors"),
    #{<<"core:configmaps">>:=1} = nkactor_namespace:get_counters("b.a.test.my_actors"),
    #{<<"core:configmaps">>:=1} = nkactor_namespace:get_counters("c.b.a.test.my_actors"),

    [
        {<<"my_actors">>, my_actors, _},
        {<<"a.test.my_actors">>, my_actors, _},
        {<<"c.b.a.test.my_actors">>, my_actors, _}
    ] = nkactor_master:get_all_namespaces(<<>>),
    [
        {<<"my_actors">>, my_actors, _},
        {<<"a.test.my_actors">>, my_actors, _},
        {<<"c.b.a.test.my_actors">>, my_actors, _}
    ] = nkactor_master:get_all_namespaces("my_actors"),
    [
        {<<"a.test.my_actors">>, my_actors, _},
        {<<"c.b.a.test.my_actors">>, my_actors, _}
    ] = nkactor_master:get_all_namespaces("test.my_actors"),

    {true, _, #actor_id{pid=C1}} = nkactor_namespace:find_actor("core:configmaps:config_c.c.b.a.test.my_actors"),
    {ok, my_actors, NS1} = nkactor_namespace:get_namespace("c.b.a.test.my_actors"),
    % If the namespace is killed, or stopped with anything other than 'normal' the actor re-registers with it, restarting it
    ok = nkactor_namespace:stop_namespace("c.b.a.test.my_actors", my_stop),
    timer:sleep(50),
    {true, _, #actor_id{pid=C1}} = nkactor_namespace:find_actor("core:configmaps:config_c.c.b.a.test.my_actors"),
    {ok, my_actors, NS2} = nkactor_namespace:get_namespace("c.b.a.test.my_actors"),
    true = NS1 /= NS2,
    % If it is stopped,
    ok = nkactor_namespace:stop_namespace("c.b.a.test.my_actors", normal),
    timer:sleep(50),
    false = nkactor_namespace:find_actor("core:configmaps:config_c.c.b.a.test.my_actors"),
    {ok, my_actors, NS3} = nkactor_namespace:get_namespace("c.b.a.test.my_actors"),
    true = NS2 /= NS3,
    ok.





list_test_1() ->
    Empty = #{},
    {ok, Empty} = search_resources("core", #{}),
    {ok, #{<<"configmaps">>:=3, <<"users">>:=1}=All} = search_resources("core", #{deep=>true}),
    {ok, Empty} = search_resources("core1", #{deep=>true}),
    {ok, All} = search_resources("core", #{namespace=>"test.my_actors", deep=>true}),
    {ok, Empty} = search_resources("core", #{namespace=>"test2.my_actors", deep=>true}),
    {ok, All} = search_resources("core", #{namespace=>"a.test.my_actors", deep=>true}),
    {ok, #{<<"configmaps">>:=1}} = search_resources("core", #{namespace=>"a.test.my_actors"}),
    {ok, #{<<"configmaps">>:=1, <<"users">>:=1}} = search_resources("core", #{namespace=>"b.a.test.my_actors"}),
    {ok, #{<<"configmaps">>:=2, <<"users">>:=1}} = search_resources("core", #{namespace=>"b.a.test.my_actors", deep=>true}),
    {ok, #{<<"configmaps">>:=1}} = search_resources("core", #{namespace=>"c.b.a.test.my_actors"}),
    {ok, #{<<"configmaps">>:=1}} = search_resources("core", #{namespace=>"c.b.a.test.my_actors", deep=>true}),

    A = "ca.a.test.my_actors",
    B = "cb.b.a.test.my_actors",
    C = "cc.c.b.a.test.my_actors",
    U = "ut1.b.a.test.my_actors",
    {ok, #actor_id{uid=A_UID}} = nkactor:find(A),
    {ok, #actor_id{uid=B_UID}} = nkactor:find(B),
    {ok, #actor_id{uid=C_UID}} = nkactor:find(C),
    {ok, #actor_id{uid=U_UID}} = nkactor:find(U),

    {ok, []} = search_linked_to(A, #{}),
    {ok, [{B_UID, <<"my_link">>}]} = search_linked_to(A, #{deep=>true}),
    {ok, []} = search_linked_to(A, #{deep=>true, link_type=>b}),
    {ok, [{B_UID, <<"my_link">>}]} = search_linked_to(A, #{deep=>true, link_type=>my_link}),
    {ok, []} = search_linked_to(B, #{}),
    {ok, [{L1, <<"my_link">>}, {L2, <<"my_link">>}]=LL1} = search_linked_to(B, #{deep=>true}),
    {ok, [{L1, <<"my_link">>}]} = search_linked_to(B, #{deep=>true, size=>1}),
    {ok, [{L2, <<"my_link">>}]} = search_linked_to(B, #{deep=>true, size=>1, from=>1}),
    S1 = lists:sort([C_UID, U_UID]),
    S1 = lists:sort([L1, L2]),
    {ok, []} = search_linked_to(B, #{namespace=>"test2.my_actors", deep=>true}),
    {ok, LL1} = search_linked_to(B, #{namespace=>"test.my_actors", deep=>true}),

    {ok, [W1, W2, W3]=W11} = search_fts(domain, #{deep=>true}),
    SW11 = lists:sort([A_UID, B_UID, C_UID]),
    SW11 = lists:sort([W1, W2, W3]),
    {ok, W11} = search_fts(<<"dómain"/utf8>>, #{deep=>true}),
    {ok, W11} = search_fts(<<"dóm*"/utf8>>, #{deep=>true}),
    {ok, [A_UID]} = search_fts(<<"dómain"/utf8>>, #{namespace=>"a.test.my_actors"}),
    {ok, [A_UID]} = search_fts(<<"dómain"/utf8>>, #{namespace=>"a.test.my_actors", field=>fts_class}),
    {ok, []} = search_fts(<<"dómain"/utf8>>, #{namespace=>"a.test.my_actors", field=>fts_class2}),


    P1 = #{
        deep => true,
        totals => true,
        filter => #{
            'and' => [#{field=><<"resource">>, op=>eq, value=><<"configmaps">>}]
        },
        sort => [#{order=>asc, field=><<"name">>}]
    },
    {ok, [#{uid:=A_UID}, #{uid:=B_UID}, #{uid:=C_UID}], #{size:=3, total:=3}} = search(P1),
    {ok, [#{uid:=B_UID}], #{size:=1, total:=3}} = search(P1#{from=>1, size=>1}),

    {ok, [#{uid:=A_UID}, #{uid:=B_UID}], #{size:=2, total:=3}} = search(P1#{size=>2}),
    {ok, [#{uid:=B_UID}, #{uid:=C_UID}], #{size:=2, total:=2}} = search(P1#{namespace=>"b.a.test.my_actors"}),

    {error, actor_has_linked_actors} = nkactor:delete(B_UID),
    {error, actor_has_linked_actors} = req(#{verb=>delete, uid=>B_UID}),

%%    D1 = #{deep=>true, filter=>#{'and' => [#{field=><<"resource">>, op=>eq, value=><<"users">>}]}, do_delete=>true},
%%    {deleted, 1} = nkactor:search_delete(my_actors, D1),
%%    {deleted, 0} = nkactor:search_delete(my_actors, D1),

    {error, {syntax_error, <<"sort.order">>}} = search(#{sort=>[#{order=>asc1, field=>"metadata.update_time"}]}),


%%    {2, 2, [DomainRoot, DomainA]} = http_list("/domains?sort=asc:metadata.updateTime"),
%%    {4, 4, [DomainRoot, DomainA, DomainB, DomainC]} = http_list("/domains?sort=asc:metadata.updateTime&deep=true"),
%%    {4, 4, [DomainC, DomainB, DomainA, DomainRoot]} = http_list("/domains/root/domains?deep=true"),
%%    {2, 2, [DomainC, DomainB]} = http_list("/domains/a-nktest/domains?deep=true"),
%%    {1, 1, [DomainB]} = http_list("/domains/a-nktest/domains?deep=false"),
%%    {1, 1, [DomainC]} = http_list("/domains/b.a-nktest/domains?deep=true"),
%%    {1, 1, [DomainC]} = http_list("/domains/b.a-nktest/domains"),
%%    {4, 2, [DomainC, DomainB]} = http_list("/domains/root/domains?deep=true&size=2"),
%%    {4, 0, []} = http_list("/domains/root/domains?deep=true&size=0"),
%%    {400, #{<<"reason">>:=<<"parameter_invalid">>}} =  http_get("/domains/root/domains?deep=true&size=-1"),
%%    {4, 3, [DomainB, DomainA, DomainRoot]} = http_list("/domains/root/domains?deep=true&size=5&from=1"),
%%
%%    {1, 1,  [Admin]} = http_list("/users"),
%%    {2, 2,  [User1, Admin]} = http_list("/users?deep=true"),
%%    {0, 0,  []} = http_list("/domains/a-nktest/users"),
%%    {1, 1,  [User1]} = http_list("/domains/a-nktest/users?deep=true"),
%%    {1, 1,  [User1]} = http_list("/domains/b.a-nktest/users"),
%%
%%    #{<<"metadata">>:=#{<<"name">>:=<<"root">>}} = DomainRoot,
%%    #{<<"metadata">>:=#{<<"domain">>:=<<"root">>, <<"name">>:=<<"a-nktest">>}} = DomainA,
%%    #{<<"metadata">>:=#{<<"domain">>:=<<"a-nktest">>, <<"name">>:=<<"b">>}} = DomainB,
%%    #{<<"metadata">>:=#{<<"domain">>:=<<"b.a-nktest">>, <<"name">>:=<<"c">>}} = DomainC,
%%    #{<<"metadata">>:=#{<<"domain">>:=<<"b.a-nktest">>, <<"name">>:=<<"ut1">>}} = User1,
%%
%%    % labels
%%    {0, 0, []} = http_list("/domains?labelSelector=is_b_domain"),
%%    {2, 2, [DomainC, DomainB]} = http_list("/domains?labelSelector=is_b_domain&deep=true"),
%%    {2, 2, [DomainB, DomainC]} = http_list("/domains?labelSelector=is_b_domain&deep=true&sort=asc:metadata.updateTime"),
%%    {1, 1, [DomainC]} = http_list("/domains?labelSelector=is_b_domain,is_c_domain&deep=true"),
%%    {0, 0, []} = http_list("/domains?labelSelector=is_b_domain,is_c_domain:false&deep=true"),
%%    {2, 2, [DomainC, DomainB]} = http_list("/domains?labelSelector=is_b_domain:true&deep=true"),
%%    {0, 0, []} = http_list("/domains?labelSelector=is_b_domain:false&deep=true"),
%%    {0, 0, []} = http_list("/domains/a-nktest/users?labelSelector=is_b_domain"),
%%    {1, 1, [User1]} = http_list("/domains/a-nktest/users?labelSelector=is_b_domain&deep=true"),
%%
%%    % links
%%    #{<<"metadata">>:=#{<<"uid">>:=DomainA_UID}} = DomainA,
%%    lager:error("NKLOG DA ~p", [DomainA_UID]),
%%    {0, 0, []} = http_list("/domains?linkedTo="++binary_to_list(DomainA_UID)),
%%    {1, 1, [DomainB]} = http_list("/domains?linkedTo="++binary_to_list(DomainA_UID)++"&deep=true"),
%%    {1, 1, [DomainB]} = http_list("/domains?linkedTo="++binary_to_list(DomainA_UID)++":io.netc.core.domain&deep=true"),
%%    {0, 0, []} = http_list("/domains?linkedTo="++binary_to_list(DomainA_UID)++":other&deep=true"),
%%
%%    % FTS
%%    {1, 1, [DomainA]} = http_list("/domains?fts=domain"),
%%    {3, 3, [DomainC, DomainB, DomainA]} = http_list("/domains?fts=domain&deep=true"),
%%    %{3, 3, [DomainC, DomainB, DomainA]} = http_list(<<"/domains?fts=dómain&deep=true"/utf8>>),
%%    % Erlang21 does not accept utf8 in url in httpc
%%    {3, 3, [DomainC, DomainB, DomainA]} = http_list("/domains?fts=dom%C3%A1in&deep=true"),
%%    {3, 3, [DomainC, DomainB, DomainA]} = http_list("/domains?fts=dom*&deep=true"),
%%    {1, 1, [DomainB]} = http_list("/domains?fts=b*&deep=true"),
%%    {0, 0, []} = http_list("/domains?fts=name:b*&deep=true"),
%%    {1, 1, [DomainB]} = http_list("/domains?fts=fts_domain:b*&deep=true"),
%%    {0, 0, []} = http_list("/users?fts=fts_domain:b*&deep=true"),
%%    {1, 1, [User1]} = http_list("/users?fts=my&deep=true"),
%%    {1, 1, [User1]} = http_list("/users?fts=fts_name:my&deep=true"),
%%    {0, 0, []} = http_list("/users?fts=fts_domain:my&deep=true"),
    ok.
%%
%%
%%search_test() ->
%%    % No apiGroup or kind, gets all objects but no special fields
%%    {400, #{<<"message">>:=<<"Field 'reason' is invalid">>}} =
%%        http_search("root", #{filter=>#{'and'=>[#{field=>reason, value=><<>>}]}}),
%%
%%    Opts1 = #{
%%        deep => true,
%%        filter => #{
%%            'and' => [#{field=>kind, op=>values, value=>['Domain', 'User']}]
%%        },
%%        sort=>[
%%            #{field=>kind, order=>asc},
%%            #{field=>'metadata.updateTime', order=>desc}
%%        ]
%%    },
%%    {6, 6, List1} = http_search("root", Opts1),
%%    [
%%        #{
%%            <<"apiVersion">> := <<"core/v1a1">>,
%%            <<"kind">> := <<"Domain">>,
%%            <<"metadata">> := #{
%%                <<"domain">> := <<"b.a-nktest">>,
%%                <<"name">> := <<"c">>
%%            }
%%        },
%%        #{
%%            <<"kind">> := <<"Domain">>,
%%            <<"metadata">> := #{
%%                <<"domain">> := <<"a-nktest">>,
%%                <<"name">> := <<"b">>
%%            }
%%        },
%%        #{
%%            <<"kind">> := <<"Domain">>,
%%            <<"metadata">> := #{
%%                <<"domain">> := <<"root">>,
%%                <<"name">> := <<"a-nktest">>
%%            }
%%        },
%%        #{
%%            <<"kind">> := <<"Domain">>,
%%            <<"metadata">> := #{
%%                <<"domain">> := <<"root">>,
%%                <<"name">> := <<"root">>,
%%                <<"uid">> := RootUID
%%            }
%%        },
%%        #{
%%            <<"kind">> := <<"User">>,
%%            <<"metadata">> := #{
%%                <<"domain">> := <<"b.a-nktest">>,
%%                <<"name">> := <<"ut1">>
%%
%%            }
%%        },
%%        #{
%%            <<"kind">> := <<"User">>,
%%            <<"metadata">> := #{
%%                <<"domain">> := <<"root">>,
%%                <<"name">> := <<"admin">>
%%
%%            }
%%        }
%%    ] = List1,
%%
%%    % apiGroup but no kind.
%%    {400, #{<<"message">>:=<<"Field 'reason' is invalid">>}} =
%%        http_search("root", #{apiGroup=>core, filter=>#{'and'=>[#{field=>reason, value=><<>>}]}}),
%%
%%    Opts2 = #{
%%        apiGroup => core,
%%        filter => #{
%%            'and' => [#{field=>kind, op=>gte, value=>'Event'}]
%%        },
%%        sort=>[
%%            #{field=>kind, order=>asc},
%%            #{field=>'metadata.updateTime', order=>desc}
%%        ]
%%    },
%%    {3, 3, List2} = http_search("root", Opts2),
%%    [
%%        #{
%%            <<"apiVersion">> := <<"core/v1a1">>,
%%            <<"kind">> := <<"Event">>,
%%            <<"involvedObject">> := #{
%%                <<"apiVersion">> := <<"core/v1a1">>,
%%                <<"domain">> := <<"root">>,
%%                <<"kind">> := <<"User">>,
%%                <<"name">> := <<"admin">>
%%            },
%%            <<"reason">> := <<"ActorCreated">>
%%        },
%%        #{
%%            <<"kind">> := <<"Event">>,
%%            <<"involvedObject">> := #{
%%                <<"domain">> := <<"root">>,
%%                <<"kind">> := <<"Domain">>,
%%                <<"name">> := <<"root">>
%%            },
%%            <<"reason">> := <<"ActorCreated">>
%%        },
%%        #{<<"apiVersion">> := <<"core/v1a1">>,
%%            <<"kind">> := <<"User">>,
%%            <<"metadata">> := #{
%%                <<"domain">> := <<"root">>,
%%                <<"name">> := <<"admin">>
%%            }
%%        }
%%    ] = List2,
%%
%%
%%    % kind and no apiGroup
%%    {400, #{<<"message">>:=<<"Missing field: 'apiGroup'">>}} = http_search("root", #{kind=>'User'}),
%%
%%    % kind and apiGroup, so we can use specific fields
%%    Opts3 = #{
%%        apiGroup => core,
%%        kind => 'Event',
%%        deep => true,
%%        filter => #{
%%            'and' => [
%%                #{field=>reason, value=>'ActorCreated'}
%%            ]
%%        },
%%        sort=>[
%%            #{field=>'involvedObject.apiVersion', order=>asc},
%%            #{field=>'involvedObject.kind', order=>asc},
%%            #{field=>'metadata.updateTime', order=>desc}
%%        ]
%%    },
%%    {3, 3, List3} = http_search("b.a-nktest", Opts3),
%%    [
%%        #{
%%            <<"kind">> := <<"Event">>,
%%            <<"involvedObject">> := #{
%%                <<"apiVersion">> := <<"core/v1a1">>,
%%                <<"kind">> := <<"Domain">>,
%%                <<"domain">> := <<"b.a-nktest">>,
%%                <<"name">> := <<"c">>
%%            },
%%            <<"reason">> := <<"ActorCreated">>
%%        },
%%        #{
%%            <<"involvedObject">> := #{
%%                <<"apiVersion">> := <<"core/v1a1">>,
%%                <<"kind">> := <<"Domain">>,
%%                <<"domain">> := <<"a-nktest">>,
%%                <<"name">> := <<"b">>
%%            }
%%        },
%%        #{
%%            <<"involvedObject">> := #{
%%                <<"apiVersion">> := <<"core/v1a1">>,
%%                <<"domain">> := <<"b.a-nktest">>,
%%                <<"kind">> := <<"User">>,
%%                <<"name">> := <<"ut1">>
%%            },
%%            <<"reason">> := <<"ActorCreated">>
%%        }
%%    ] = List3,
%%
%%    % labels
%%    OptsL1 = #{
%%        filter => #{'and' => [
%%            #{field=>'kind', value=>'Domain'},
%%            #{field=>'metadata.labels.is_b_domain', op=>exists, value=>true}
%%        ]}
%%    },
%%    {0, 0, []} = http_search("root", OptsL1),
%%    {2, 2, [#{<<"metadata">>:=#{<<"name">>:=<<"c">>}}=C, #{<<"metadata">>:=#{<<"name">>:=<<"b">>}}=B]} =
%%        http_search("root", OptsL1#{deep=>true}),  % B and C
%%    % we can also filter using apiGroup and kind
%%    OptsL2 = fun(V) ->
%%        #{
%%            apiGroup => core,
%%            kind => 'Domain',
%%            deep => true,
%%            filter => #{'and' => [
%%                #{field=>'metadata.labels.is_b_domain', op=>eq, value=>V}
%%            ]},
%%            sort => [#{field=>'metadata.updateTime', order=>asc}]
%%        }
%%    end,
%%    {2, 2, [B, C]} = http_search("root", OptsL2(<<"true">>)),
%%    {0, 0, []} = http_search("root", OptsL2(<<"false">>)),
%%
%%    % links
%%    OptsI1 = #{
%%        filter => #{'and' => [
%%            #{field=>'kind', value=>'Domain'},
%%            #{field=><<"metadata.links.", RootUID/binary>>, op=>exists, value=>true}
%%        ]}
%%    },
%%    {1, 1, [A]} = http_search("root", OptsI1),
%%    #{<<"metadata">>:=#{<<"name">>:=<<"a-nktest">>, <<"uid">>:=_UID_A}} = A,
%%    OptsI2 = #{
%%        deep => true,
%%        filter => #{'and' => [
%%            #{field=><<"metadata.links.", RootUID/binary>>, value=><<"io.netc.core.domain">>}
%%        ]},
%%        sort => [#{field=>'metadata.updateTime', order=>asc}]
%%    },
%%    {2, 2, [Admin, A]} = http_search("root", OptsI2),
%%    #{<<"metadata">>:=#{<<"name">>:=<<"admin">>}} = Admin,
%%
%%    % FTS
%%    OptsF1 = fun(D, F, Op, V) ->
%%        #{
%%            deep => D,
%%            filter => #{'and' => [
%%                #{field=>'kind', value=>'Domain'},
%%                #{field=>F, op=>Op, value=>V}
%%            ]}
%%        }
%%    end,
%%    {1, 1, [A]} = http_search("root", OptsF1(false, 'metadata.fts.*', eq, <<"Domaín"/utf8>>)),
%%    {3, 3, [C,B,A]} = http_search("root", OptsF1(true, 'metadata.fts.*', eq, <<"domain">>)),
%%    {3, 3, [C,B,A]} = http_search("root", OptsF1(true, 'metadata.fts.*', prefix, <<"dOm">>)),
%%    {1, 1, [B]} = http_search("root", OptsF1(true, 'metadata.fts.fts_domain', prefix, <<"b">>)),
%%    OptsF2 = fun(D, F, Op, V) ->
%%        #{
%%            deep => D,
%%            apiGroup => core,
%%            kind => 'User',
%%            filter => #{'and' => [
%%                #{field=>F, op=>Op, value=>V}
%%            ]}
%%        }
%%    end,
%%    {0, 0, []} = http_search("root", OptsF2(true, 'metadata.fts.fts_domain', prefix, <<"b">>)),
%%    {0, 0, []} = http_search("root", OptsF2(true, 'metadata.fts.fts_domain', prefix, <<"b">>)),
%%    {1, 1, [#{<<"metadata">>:=#{<<"name">>:=<<"ut1">>}}=U1]} = http_search("root", OptsF2(true, 'metadata.fts.*', eq, <<"my">>)),
%%
%%    % delete
%%    OptsD1 = #{
%%        apiGroup => core,
%%        kind => 'User'
%%    },
%%    {1, 1, [U1]} = http_search("b.a-nktest", OptsD1),
%%    {200, #{<<"details">>:=#{<<"deleted">>:=1}}} = http_search_delete("b.a-nktest", OptsD1),
%%    {200, #{<<"details">>:=#{<<"deleted">>:=0}}} = http_search_delete("b.a-nktest", OptsD1),
%%    {0, 0, []} = http_search("b.a-nktest", OptsD1),
%%    ok.
%%
%%
%%contact_test() ->
%%    % Create an contact
%%    Body1 = <<"
%%        apiVersion: core/v1a1
%%        kind: Contact
%%        spec:
%%            name: 'My Náme'
%%            surname: 'My Surname'
%%            birthTime: 2018-01-01
%%            gender: M
%%            timezone: -1
%%            url:
%%                - url: url1
%%                - url: url2
%%                  type: type2
%%                  meta:
%%                    a: 1
%%            phone:
%%                - type: mobile
%%                  phone: 123456
%%                - type: fixed
%%                  phone: 654321
%%            email:
%%                email: test@test.com
%%            im:
%%                - type: irc
%%                  im: abc
%%            address:
%%                - type: home
%%                  street: 'My street'
%%                  code: 1234
%%                  country: Spain
%%            pubkey:
%%                - type: github
%%                  key: abcde
%%                  meta:
%%                    key1: val1
%%            profile:
%%                - type: type1
%%                  startTime: 2017-01
%%                  stopTime: 2018-02
%%                  data:
%%                    data1: val1
%%                  meta:
%%                    meta1: val1
%%            photo:
%%                - type: type2
%%                  file: file2
%%                  meta:
%%                    meta2: val2
%%            user: /apis/core/v1a1/domains/b.a-nktest/users/ut1
%%        metadata:
%%            name: ct1
%%            domain: c.b.a-nktest
%%            fts:
%%                fullName: 'My Náme My Surname'
%%    "/utf8>>,
%%    Body2 = yaml(Body1),
%%
%%    {created, CT1} = req(#{verb=>create, body=>Body2}),
%%    {ok, #{<<"metadata">>:=#{<<"uid">>:=C_B_A_UID}}} = req(#{resource=>"domains", domain=>"b.a-nktest", name=>"c"}),
%%    {ok, #{<<"metadata">>:=#{<<"uid">>:=UT1_UID}}} = req(#{resource=>"users", domain=>"b.a-nktest", name=>"ut1"}),
%%    #{
%%        <<"apiVersion">> := <<"core/v1a1">>,
%%        <<"kind">> := <<"Contact">>,
%%        spec := #{
%%            <<"user">> := <<"/apis/core/v1a1/domains/b.a-nktest/users/ut1">>,
%%            <<"name">> := <<"My Náme"/utf8>>,
%%            <<"surname">> := <<"My Surname">>,
%%            <<"normalizedName">> := <<"my name">>,
%%            <<"normalizedSurname">> := <<"my surname">>,
%%            <<"birthTime">> := <<"2018-01-01T00:00:00Z">>,
%%            <<"gender">> := <<"M">>,
%%            <<"timezone">> := -1,
%%            <<"url">> := [
%%                #{<<"url">> := <<"url1">>},
%%                #{
%%                    <<"meta">> := #{<<"a">> := 1},
%%                    <<"type">> := <<"type2">>,
%%                    <<"url">> := <<"url2">>
%%                }
%%            ],
%%            <<"phone">> := [
%%                #{
%%                    <<"type">> := <<"mobile">>,
%%                    <<"phone">> := <<"123456">>
%%                },
%%                #{
%%                    <<"type">> := <<"fixed">>,
%%                    <<"phone">> := <<"654321">>
%%                }
%%            ],
%%            <<"email">> := [
%%                #{<<"email">> := <<"test@test.com">>}
%%            ],
%%            <<"im">> := [
%%                #{
%%                    <<"type">> := <<"irc">>,
%%                    <<"im">> := <<"abc">>
%%                }
%%            ],
%%            <<"address">> := [
%%                #{
%%                    <<"type">> := <<"home">>,
%%                    <<"street">> := <<"My street">>,
%%                    <<"code">> := <<"1234">>,
%%                    <<"country">> := <<"Spain">>
%%                }
%%            ],
%%            <<"pubkey">> := [
%%                #{
%%                    <<"type">> := <<"github">>,
%%                    <<"key">> := <<"abcde">>,
%%                    <<"meta">> := #{<<"key1">> := <<"val1">>}
%%                }
%%            ],
%%            <<"profile">> := [
%%                #{
%%                    <<"type">> := <<"type1">>,
%%                    <<"data">> := #{<<"data1">> := <<"val1">>},
%%                    <<"meta">> := #{<<"meta1">> := <<"val1">>},
%%                    <<"startTime">> := <<"2017-01-01T00:00:00Z">>,
%%                    <<"stopTime">> := <<"2018-02-01T00:00:00Z">>
%%                }
%%            ],
%%            <<"photo">> := [
%%                #{
%%                    <<"type">> := <<"type2">>,
%%                    <<"file">> := <<"file2">>,
%%                    <<"meta">> := #{<<"meta2">> := <<"val2">>}
%%                }
%%            ]
%%        }=Spec1,
%%        <<"metadata">> := #{
%%            <<"uid">> := C1_UID,
%%            <<"domain">> := <<"c.b.a-nktest">>,
%%            <<"name">> := <<"ct1">>,
%%            creation_time := <<"20", _/binary>> = T1,
%%            update_time := <<"20", _/binary>> = T1,
%%            <<"generation">> := 0,
%%            hash := Rs1,
%%            <<"selfLink">> := <<"/apis/core/v1a1/domains/c.b.a-nktest/contacts/ct1">>,
%%            <<"links">> := #{
%%                C_B_A_UID := <<"io.netc.core.domain">>,
%%                UT1_UID := <<"io.netc.core.contact-user">>
%%            },
%%            <<"fts">> := #{
%%                <<"fullName">> := <<"My Náme My Surname"/utf8>>
%%            }
%%        },
%%        <<"status">> := #{<<"isActivated">>:=true}
%%    } = CT1,
%%
%%    {error, #{<<"reason">>:= <<"uniqueness_violation">>}} = req(#{verb=>create, body=>Body2}),
%%
%%    Spec2 = maps:remove(<<"im">>, Spec1),
%%    Body3 = maps:remove(<<"status">>, CT1#{spec:=Spec2}),
%%    {ok, CT2} = req(#{verb=>update, body=>Body3}),
%%    #{
%%        spec := Spec2,
%%        <<"metadata">> := #{
%%            <<"uid">> := C1_UID,
%%            creation_time := T1,
%%            update_time := T2,
%%            <<"generation">> := 1,
%%            hash := Rs2
%%        }
%%    } = CT2,
%%    true = Rs1 /= Rs2,
%%    true = T2 > T1,
%%
%%    {error, #{<<"message">>:= <<"Field 'kind' is invalid">>}} = req(#{verb=>update, resource=>users, body=>Body3}),
%%    {error, #{<<"message">>:= <<"Field 'metadata.name' is invalid">>}} = req(#{verb=>update, name=>name2, body=>Body3}),
%%    {error, #{<<"message">>:= <<"Field 'metadata.domain' is invalid">>}} = req(#{verb=>update, domain=>"a-nktest", body=>Body3}),
%%    {ok, _} = req(#{verb=>update, domain=>"c.b.a-nktest", resource=>"contacts", name=>"ct1", body=>Body3}),
%%
%%    {1, 1, [#{<<"metadata">>:=#{<<"uid">>:=C1_UID}}=CT3]} = http_list("/domains/c.b.a-nktest/contacts?linkedTo="++binary_to_list(UT1_UID)++":io.netc.core.contact-user"),
%%    {0, 0, []} = http_list("/domains/c.b.a-nktest/contacts?linkedTo="++binary_to_list(UT1_UID)++":1"),
%%    {1, 1, [CT3]} = http_list("/domains/c.b.a-nktest/contacts?fieldSelector=spec.gender:M&sort=spec.timezone"),
%%    {0, 0, []} = http_list("/domains/c.b.a-nktest/contacts?fieldSelector=spec.gender:F"),
%%    {1, 1, [CT3]} = http_list("/domains/c.b.a-nktest/contacts?fieldSelector=spec.gender:M,spec.birthTime:gt:2007"),
%%    {0, 0, []} = http_list("/domains/c.b.a-nktest/contacts?fieldSelector=spec.gender:M,spec.birthTime:gt:2020"),
%%
%%%%    {422, _} = http_delete("/domains/b.a-nktest/users/ut1"),
%%%%
%%%%    % Remove link
%%%%    Body4 = maps:remove(<<"status">>, CT1#{<<"metadata">>:=maps:remove(<<"links">>, Meta1)}),
%%%%    {ok, _} = api(#{verb=>update, domain=>"c.b.a-nktest", resource=>"contacts", name=>"ct1", body=>Body4}),
%%%%    {0, 0, []} = http_list("/domains/c.b.a-nktest/contacts?linkedTo=user:"++binary_to_list(UT1_UID)),
%%%%    {200, _} = http_delete("/domains/b.a-nktest/users/ut1"),
%%%%    {error, #{<<"reason">>:=<<"linked_actor_unknown">>}} = api(#{verb=>create, body=>Body2}),
%%%%    {200, _} = http_delete("/domains/c.b.a-nktest/contacts/ct1"),
%%    ok.
%%
%%
%%event_test() ->
%%    nkactor_core_test_util:delete_test_data(),
%%    nkdomain_api_events:remove_old_hashes(0),
%%    no_message = clean_events(),
%%    % delete_test_data(),
%%    Start = nklib_date:now_3339(msecs),
%%
%%    #{d1:=D1, d2:=D2, d3:=D3, u1:=U1} = nkactor_core_test_util:test_data(),
%%    {ok, #{<<"metadata">>:=RootMeta}} = req(#{resource=>"domains", name=>"root"}),
%%    #{<<"uid">>:=RootUID} = RootMeta,
%%    {ok, 'nkdomain-root', RootUID} = nkdomain_register:get_domain_data("root"),
%%
%%    % Watch on root domain
%%    WatchRoot = http_watch("/domains/root"),
%%
%%    % Create domain NkTest
%%    {created, A1} = req(#{verb=>create, resource=>domains, name=>"a-nktest", body=>D1}),
%%    #{<<"metadata">> := #{<<"uid">> := A_UID, hash:=AVsn}} = A1,
%%
%%    [{{nkdomain_api_http, Listen1}, _}] = nkdomain_api_core:get_watches(),
%%    #actor_id{group=?GROUP_CORE, resource=?RES_CORE_DOMAINS, name= <<"root">>} = Listen1,
%%
%%    % Event is generated at 'a' domain, but no one is watching yet
%%    % It is also sent to a's domain, root where we are watching
%%    {<<"ADDED">>, Ev1} = wait_http_event(WatchRoot, <<"ActorCreated">>),
%%
%%    #{
%%        <<"apiVersion">> := <<"core/v1a1">>,
%%        <<"kind">> := <<"Event">>,
%%        <<"type">> := <<"Normal">>,
%%        <<"reason">> := <<"ActorCreated">>,
%%        <<"involvedObject">> := #{
%%            <<"domain">> := <<"root">>,
%%            <<"kind">> := <<"Domain">>,
%%            <<"name">> := <<"a-nktest">>,
%%            <<"uid">> := A_UID,
%%            hash := AVsn
%%        },
%%        <<"message">> := <<>>,
%%        <<"metadata">> := #{
%%            <<"domain">> := <<"a-nktest">>,
%%            <<"generation">> := 0,
%%            <<"name">> := Ev1Name,
%%            hash := _,
%%            <<"selfLink">> := <<"/apis/core/v1a1/domains/a-nktest/events/", Ev1Name/binary>>,
%%            <<"uid">> := _Ev1UID,
%%            creation_time := CT1,
%%            update_time := CT1
%%        } = _Meta1,
%%        <<"count">> := 1,
%%        <<"firstTimestamp">> := Time1,
%%        <<"lastTimestamp">> := Time1,
%%        <<"source">> := #{
%%            <<"component">> := <<"nkdomain">>,
%%            <<"host">> := _
%%        }
%%    } = Ev1,
%%
%%    ok = nkdomain_api_events:wait_for_save(),
%%    {ok, #{<<"items">>:=[Ev1]}} =
%%        req(#{verb=>list, resource=>"events", domain=>"a-nktest", params=>#{fieldSelector=><<"involvedObject.uid:", A_UID/binary>>}}),
%%
%%    % Now 'root' domain generates an event
%%    nkactor:async_op("/root/core/domains/root", {send_event, test_api}),
%%    {<<"ADDED">>, Ev2} = wait_http_event(WatchRoot, <<"TestAPI">>),
%%    #{
%%        <<"reason">> := <<"TestAPI">>,
%%        <<"involvedObject">> := #{
%%            <<"domain">> := <<"root">>,
%%            <<"kind">> := <<"Domain">>,
%%            <<"name">> := <<"root">>,
%%            <<"uid">> := RootUID
%%        },
%%        <<"metadata">> := #{
%%            <<"domain">> := <<"root">>,
%%            <<"generation">> := 0,
%%            <<"name">> := Ev2Name,
%%            hash := Ev2RV,
%%            <<"selfLink">> := <<"/apis/core/v1a1/domains/root/events/", Ev2Name/binary>>,
%%            <<"uid">> := Ev2UID,
%%            creation_time := CT2,
%%            update_time := CT2
%%        },
%%        <<"count">> := 1,
%%        <<"firstTimestamp">> := Time2,
%%        <<"lastTimestamp">> := Time2
%%    } = Ev2,
%%
%%    ok = nkdomain_api_events:wait_for_save(),
%%    {ok, Ev1} = req(#{resource=>events, domain=>"a-nktest", name=>Ev1Name, params=>#{activate=>false}}),
%%
%%    nkactor:async_op("/root/core/domains/root", {send_event, test_api}),
%%    {<<"MODIFIED">>, Ev3} = wait_http_event(WatchRoot, <<"TestAPI">>),
%%    #{
%%        <<"reason">> := <<"TestAPI">>,
%%        <<"involvedObject">> := #{
%%            <<"uid">> := RootUID
%%        },
%%        <<"metadata">> := #{
%%            <<"domain">> := <<"root">>,
%%            <<"generation">> := 1,
%%            <<"name">> := Ev2Name,
%%            hash := Ev3RV,
%%            <<"selfLink">> := <<"/apis/core/v1a1/domains/root/events/", Ev2Name/binary>>,
%%            <<"uid">> := Ev2UID,
%%            creation_time := CT2,
%%            update_time := CT3
%%        },
%%        <<"count">> := 2,
%%        <<"firstTimestamp">> := Time2,
%%        <<"lastTimestamp">> := Time3
%%    } = Ev3,
%%    true = CT3 > CT2,
%%    true = Time3 > CT2,
%%    true = Ev2RV /= Ev3RV,
%%
%%    % Listen on domain a-nktest, and again only for user events
%%    WatchA = api_watch(#{resource=>domains, name=>"a-nktest"}),
%%    WatchAU = api_watch(#{domain=>"a-nktest", resource=>users}),
%%    timer:sleep(100),
%%    [
%%        {{nkdomain_api_http, Listen1}, _},
%%        {{nkdomain_test_util, Listen2}, _},
%%        {{nkdomain_test_util, Listen3}, _}
%%    ] = lists:sort(nkdomain_api_core:get_watches()),
%%
%%    % Domain A generates an event
%%    % The same event is sent to itself and it's domain, "root"
%%    nkactor:async_op("/root/core/domains/a-nktest", {send_event, test_api}),
%%
%%    {<<"ADDED">>, Ev4} = wait_api_event(WatchA, <<"TestAPI">>),
%%    % Domain receives a copy of the event
%%    {<<"ADDED">>, Ev4} = wait_http_event(WatchRoot, <<"TestAPI">>),
%%
%%
%%    % Create b.a-nktest
%%    {created, B_A} = req(#{verb=>create, domain=>"a-nktest", resource=>"domains", name=>"b", body=>D2}),
%%    #{<<"metadata">> := #{<<"uid">> := B_A_UID}} = B_A,
%%
%%    % 'a' receives a copy of the event as its domain, and also escalates it to it's domain, "root"
%%    {<<"ADDED">>, Ev5} = wait_api_event(WatchA, <<"ActorCreated">>),
%%    #{<<"involvedObject">> := #{<<"uid">> := B_A_UID}} = Ev5,
%%    {<<"ADDED">>, Ev5} = wait_http_event(WatchRoot, <<"ActorCreated">>),
%%
%%    ok = nkdomain_api_events:wait_for_save(),
%%    FS_B_A_UID = <<"involvedObject.uid:", B_A_UID/binary>>,
%%
%%    {ok, #{<<"items">>:=[Ev5]}} =
%%        req(#{verb=>list, resource=>"events", params=>#{deep=>true, fieldSelector=>FS_B_A_UID}}),
%%    {ok, #{<<"items">>:=[]}} =
%%        req(#{verb=>list, resource=>"events", params=>#{fieldSelector=>FS_B_A_UID}}),
%%    {ok, #{<<"items">>:=[Ev5]}} =
%%        req(#{verb=>list, domain=>"b.a-nktest", resource=>"events", params=>#{fieldSelector=>FS_B_A_UID}}),
%%
%%
%%    % Create c.b.a-nktest
%%    {created, C_B_A} = req(#{verb=>create, domain=>"b.a-nktest", resource=>"domains", name=>"c", body=>D3}),
%%    #{<<"metadata">> := #{<<"uid">> := C_B_A_UID}} = C_B_A,
%%    {<<"ADDED">>, Ev6} = wait_api_event(WatchA, <<"ActorCreated">>),
%%    #{<<"involvedObject">> := #{<<"uid">> := C_B_A_UID}} = Ev6,
%%    {<<"ADDED">>, Ev6} = wait_http_event(WatchRoot, <<"ActorCreated">>),
%%
%%    % Now c.b.a-nktest sends an event
%%    % It is generated at itself, and copied to its fathers b.a-nktest (no listen), a and root
%%    nkactor:async_op("/b.a-nktest/core/domains/c", {send_event, test_api}),
%%    {<<"ADDED">>, Ev6b} = wait_api_event(WatchA, <<"TestAPI">>),
%%    {<<"ADDED">>, Ev6b} = wait_http_event(WatchRoot, <<"TestAPI">>),
%%
%%    % Create ut1
%%    {created, U1_B_A} = req(#{verb=>create, domain=>"b.a-nktest", resource=>users, name=>"ut1", body=>U1}),
%%    #{<<"metadata">> := #{<<"uid">> := U1_B_A_UID}} = U1_B_A,
%%    {<<"ADDED">>, Ev7} = wait_api_event(WatchA, <<"ActorCreated">>),
%%    {<<"ADDED">>, Ev7} = wait_api_event(WatchAU, <<"ActorCreated">>),
%%    #{<<"involvedObject">> := #{<<"uid">> := U1_B_A_UID}} = Ev7,
%%    {<<"ADDED">>, Ev7} = wait_http_event(WatchRoot, <<"ActorCreated">>),
%%    %#{<<"involvedObject">>:=#{<<"isActivated">>:=true}} = Ev7,
%%
%%    % Listen on U1
%%    WatchU1 = api_watch(#{domain=>"b.a-nktest", resource=>users, name=>ut1}),
%%    timer:sleep(100),
%%    [
%%        {{nkdomain_api_http, Listen1}, _},
%%        {{nkdomain_test_util, _Listen4}, _},
%%        {{nkdomain_test_util, Listen2}, _},
%%        {{nkdomain_test_util, Listen3}, _}
%%    ] = lists:sort(nkdomain_api_core:get_watches()),
%%
%%    % Now U1 sends an event
%%    nkactor:async_op("/b.a-nktest/core/users/ut1", {send_event, test_api}),
%%
%%    % It is generated at itself, and copied to its fathers b.a-nktest (no listen), a and root
%%    {<<"ADDED">>, Ev8} = wait_api_event(WatchU1, <<"TestAPI">>),
%%    {<<"ADDED">>, Ev8} = wait_api_event(WatchA, <<"TestAPI">>),
%%    {<<"ADDED">>, Ev8} = wait_api_event(WatchAU, <<"TestAPI">>),
%%    {<<"ADDED">>, Ev8} = wait_http_event(WatchRoot, <<"TestAPI">>),
%%    %#{<<"involvedObject">>:=#{<<"isActivated">>:=true}} = Ev8,
%%    {true, _} = nkservice_actor_db:is_activated("/b.a-nktest/core/users/ut1"),
%%
%%    nkdomain_api_events:wait_for_save(),    % Ensure it is just saved
%%
%%    % Let's start a new watch, over 'b' domain, but only for events after Ev6b
%%    % We need deep because that resource belongs to an event in "c"
%%    #{<<"metadata">>:=#{hash:=Ev6bRV}} = Ev6b,
%%    WatchB = api_watch(#{domain=>"a-nktest", resource=>domains, name=>"b", params=>#{deep=>true, resourceVersion=>Ev6bRV}}),
%%    timer:sleep(100),
%%    5 = length(nkdomain_api_core:get_watches()),
%%
%%    % Now we send a complicated event. The watcher is now waiting for next save, but
%%    % also listening on B. This event will be received immediately in B,
%%    % and also will appear in DB search, but it is filtered (check in the log 'filtered db event')
%%    % and only 1 will be received
%%    nkactor:async_op("/a-nktest/core/domains/b", {send_event, test_api}),
%%
%%    % We get the events already past, after Ev6bRV
%%    {<<"ADDED">>, Ev7} = wait_api_event(WatchB, <<"ActorCreated">>),
%%    {<<"ADDED">>, Ev8} = wait_api_event(WatchB, <<"TestAPI">>),
%%
%%    % We get then 'complicated' event, only once
%%    {<<"ADDED">>, Ev6c} = wait_api_event(WatchA, <<"TestAPI">>),
%%    {<<"ADDED">>, Ev6c} = wait_http_event(WatchRoot, <<"TestAPI">>),
%%    {<<"ADDED">>, Ev6c} = wait_api_event(WatchB, <<"TestAPI">>),
%%
%%
%%    % Perform an update
%%    Upd1 = #{spec => #{<<"password">>=><<"pass2">>}},
%%    {ok, _} = req(#{verb=>update, domain=>"b.a-nktest", resource=>users, name=>ut1, body=>Upd1}),
%%    {<<"ADDED">>, Ev9} = wait_api_event(WatchU1, <<"ActorUpdated">>),
%%    {<<"ADDED">>, Ev9} = wait_api_event(WatchA, <<"ActorUpdated">>),
%%    {<<"ADDED">>, Ev9} = wait_api_event(WatchAU, <<"ActorUpdated">>),
%%    {<<"ADDED">>, Ev9} = wait_http_event(WatchRoot, <<"ActorUpdated">>),
%%    {<<"ADDED">>, Ev9} = wait_api_event(WatchB, <<"ActorUpdated">>),
%%    %#{<<"involvedObject">>:=#{<<"isActivated">>:=true}} = Ev9,
%%
%%    % Perform a delete
%%    timer:sleep(100),   % Wait for the update, the delete is quicker
%%    {ok, _} = req(#{verb=>delete, domain=>"b.a-nktest", resource=>users, name=>ut1}),
%%    {<<"ADDED">>, Ev10} = wait_api_event(WatchU1, <<"ActorDeleted">>),
%%    {<<"ADDED">>, Ev10} = wait_api_event(WatchA, <<"ActorDeleted">>),
%%    {<<"ADDED">>, Ev10} = wait_api_event(WatchAU, <<"ActorDeleted">>),
%%    {<<"ADDED">>, Ev10} = wait_http_event(WatchRoot, <<"ActorDeleted">>),
%%    {<<"ADDED">>, Ev10} = wait_api_event(WatchB, <<"ActorDeleted">>),
%%    %#{<<"involvedObject">>:=#{<<"isActivated">>:=true}} = Ev10,
%%
%%    ok = nkdomain_api_events:wait_for_save(),
%%    FS_U1_B_A_UID = <<"involvedObject.uid:", U1_B_A_UID/binary>>,
%%
%%    {ok, #{<<"items">>:=[Ev10,Ev9,Ev8,Ev7]}} =
%%        req(#{verb=>list, resource=>"events", params=>#{deep=>true, fieldSelector=> FS_U1_B_A_UID}}),
%%    {ok, #{<<"items">>:=[]}} =
%%        req(#{verb=>list, resource=>"events", params=>#{fieldSelector=>FS_U1_B_A_UID}}),
%%    {ok, #{<<"items">>:=[]}} =
%%        req(#{verb=>list, domain=>"a-nktest", resource=>"events", params=>#{fieldSelector=>FS_U1_B_A_UID}}),
%%    {ok, #{<<"items">>:=[Ev10,Ev9,Ev8,Ev7]}} =
%%        req(#{verb=>list, domain=>"b.a-nktest", resource=>"events", params=>#{fieldSelector=>FS_U1_B_A_UID}}),
%%
%%
%%    timer:sleep(100),
%%    4 = length(nkdomain_api_core:get_watches()),
%%
%%    % Create /b.a-nktest/users/ut1, without activation
%%    {created, _} = req(#{verb=>create, domain=>"b.a-nktest", resource=>users, name=>"ut1",
%%        body=>U1, params=>#{activate=>false}}),
%%
%%    {<<"ADDED">>, Ev11} = wait_api_event(WatchA, <<"ActorCreated">>),
%%    % lager:error("NKLOG E11 ~p", [Ev11]),
%%    % Add sleep here?
%%    {<<"ADDED">>, Ev11} = wait_api_event(WatchAU, <<"ActorCreated">>),
%%    {<<"ADDED">>, Ev11} = wait_http_event(WatchRoot, <<"ActorCreated">>),
%%    {<<"ADDED">>, Ev11} = wait_api_event(WatchB, <<"ActorCreated">>),
%%    false = nkservice_actor_db:is_activated("/b.a-nktest/core/users/ut1"),
%%
%%    % Perform a delete over a non-activated object
%%    % resourceVersion in involvedObject would be <<>> for non-activated objects
%%    {ok, _} = req(#{verb=>delete, domain=>"b.a-nktest", resource=>users, name=>ut1}),
%%    {<<"ADDED">>, Ev12} = wait_api_event(WatchA, <<"ActorDeleted">>),
%%    {<<"ADDED">>, Ev12} = wait_api_event(WatchAU, <<"ActorDeleted">>),
%%    {<<"ADDED">>, Ev12} = wait_http_event(WatchRoot, <<"ActorDeleted">>),
%%    {<<"ADDED">>, Ev12} = wait_api_event(WatchB, <<"ActorDeleted">>),
%%
%%    % Ensure no event is waiting
%%    no_message = clean_events(),
%%    http_watch_stop(WatchRoot),
%%    api_watch_stop(WatchA),
%%    api_watch_stop(WatchAU),
%%    api_watch_stop(WatchU1),
%%    api_watch_stop(WatchB),
%%    timer:sleep(500),
%%    [] = nkdomain_api_core:get_watches(),
%%
%%    % Comment from here to leave data
%%    http_delete("/domains/root/domains/a-nktest?cascade=true"),
%%
%%    nkdomain_api_events:wait_for_save(),
%%    Opts = #{
%%        filter => #{
%%            'and' => [
%%                #{field => <<"metadata.updateTime">>, op => gt, value => Start},
%%                #{field => <<"resource">>, value => <<"events">>}
%%            ]
%%        },
%%        domain => <<"a-nktest">>,
%%        size => 100
%%    },
%%
%%    {ok, _Ids, #{total:=15}} = nkactor:search(?ROOT_SRV, Opts#{deep=>true}, #{}),
%%
%%    % No real delete yet
%%    {ok, 3, _} = nkactor:delete_all(?ROOT_SRV, Opts#{deep=>false}, #{}),
%%    {ok, 15, _} = nkactor:delete_all(?ROOT_SRV, Opts#{deep=>true}, #{}),
%%    Now = nklib_date:now_3339(msecs),
%%    {ok, 3, _} = nkactor:delete_old(?ROOT_SRV, "a-nktest", ?GROUP_CORE, ?RES_CORE_EVENTS,
%%        Now, #{}),
%%    {ok, 12, _} = nkdomain:delete_old_events(?ROOT_SRV, "a-nktest", Now),
%%    {ok, [], #{total:=0}} = nkactor:search(?ROOT_SRV, Opts#{deep=>true}, #{}),
%%
%%    Opts2 = #{filter => #{'and'=>[#{field=>'reason', value=>'TestAPI'}]}},
%%    {ok, 1, _} = nkactor:delete_all(?ROOT_SRV, Opts2, #{}),
%%    % Deletion of events don't generate new events
%%    {deleted, 1, _} = nkactor:delete_all(?ROOT_SRV, Opts2#{delete=>true}, #{}),
%%    {ok, 0, _} = nkactor:delete_all(?ROOT_SRV, Opts2, #{}),
%%    ok.
%%
%%
%%
%%
%%%% ===================================================================
%%%% Util
%%%% ===================================================================
%%
%%
%%

search_resources(Group, Opts) ->
    nkactor:search_resources(my_actors, Group, Opts).

search_linked_to(Id, Opts) ->
    nkactor:search_linked_to(my_actors, Id, Opts).

search_fts(Word, Opts) ->
    nkactor:search_fts(my_actors, Word, Opts).

search(Spec) ->
    nkactor:search_actors(my_actors, Spec).

