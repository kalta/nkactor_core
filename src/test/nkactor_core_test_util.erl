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

%% @doc Test Utilities
-module(nkactor_core_test_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').


-compile(export_all).
-compile(nowarn_export_all).

-include("nkactor_core.hrl").

-define(PGSQL_SRV, test_pgsql).
-define(ACTOR_SRV, test_actors).
-define(NAMESPACE, <<"test.my_actors">>).
-define(TOKEN, <<"my_token">>).

-define(LISTEN, "http://127.0.0.1:9001").


%% ===================================================================
%% Start and stop
%% ===================================================================

start() ->
    PgSqlConfig = #{
        targets => [#{url=>"tcp://root@127.0.0.1:26257"}],
        flavour => cockroachdb,
        database => nkactor_test,
        debug => true,
        plugins => [nkserver_ot],
        opentrace_filter => opentrace_filter()
    },
    ActorsConfig = #{
        base_namespace => ?NAMESPACE,
        plugins => [
            nkactor_core,
            nkactor_store_pgsql,
            nkactor_core_store_pgsql,
            nkactor_kapi,
            nkactor_core_kapi
        ],
        pgsql_service => ?PGSQL_SRV,
        opentrace_filter => opentrace_filter(),
        use_module => ?MODULE
    },
    RestConfig = #{
        url => ?LISTEN,
        use_module => ?MODULE,
        plugins => [nkserver_ot],
        opentrace_filter => opentrace_filter()
    },
    {ok, _} = nkserver:start_link(nkpgsql, test_pgsql, PgSqlConfig),
    {ok, _} = nkserver:start_link(nkactor, test_actors, ActorsConfig),
    {ok, _} = nkserver:start_link(nkrest, test_rest, RestConfig).



stop() ->
    nkserver:stop(test_rest),
    nkserver:stop(test_actors),
    nkserver:stop(test_pgsql).



opentrace_filter() ->
    "
        my_spans() -> send.
        count() -> count('request', span_name, final_result).
    ".


%% ===================================================================
%% Callbacks
%% ===================================================================

http_request(Verb, Path, Req, _State) ->
    nkactor_kapi:http_request(test_actors, Verb, Path, Req).


actor_authorize(#{auth:=#{token:=?TOKEN}}=Req) ->
    {true, Req};

actor_authorize(_Req) ->
    true.


%% ===================================================================
%% Util
%% ===================================================================


create_test_data() ->
    d2(),
    #{d1:=D1, d2:=D2, d3:=D3, u1:=U1} = test_data(),
    % delete_test_data(),
    {created, _} = req(#{verb=>create, body=>D1}),
    {created, _} = req(#{verb=>create, body=>D2}),
    {created, _} = req(#{verb=>create, body=>D3}),
    {created, _} = req(#{verb=>create, body=>U1}),
    ok.


d2() ->
    {ok, _} = nkactor:delete_multi(?ACTOR_SRV, #{namespace=>?NAMESPACE, deep=>true}, #{}).


delete_test_data() ->

    % First delete the events (potentially a lot) without transactions:
    http_delete("/domains/root/events?deep=true&fieldSelector=path:prefix:a-nktest"),
    % Deletes everything linked to 'a' with a transaction cascade delete
    http_delete("/domains/root/domains/a-nktest?cascade=true"),
    % Wait for events to be saved
    nkdomain_api_events:wait_for_save(),
    % Delete events generated on deletion of objects
    http_delete("/domains/root/events?deep=true&fieldSelector=path:prefix:a-nktest"),
    ok.




%% Creates:
%% - ca  @ a.test.my_actors -> core:configmaps:ca.a.test.my_actors
%% - cb  @ b.a.test.my_actors -> core:configmaps:cb.b.a.test.my_actors (linked to ca)
%% - cc  @ c.b.a.test.my_actors -> core:configmaps:cc.c.b.a.test.my_actors (linked to cb)
%% - ut1 @ b.a.test.my_actors -> core:users:ut1.b.a.test.my_actors (linked to cb)



test_data() ->
    D1 = #{
        resource => configmaps,
        name => ca,
        namespace => 'a.test.my_actors',
        metadata => #{
            labels => #{is_a => true},
            fts => #{fts_class => <<"Domáin a"/utf8>>}
        }
    },
    D2 = #{
        resource => configmaps,
        name => cb,
        namespace => 'b.a.test.my_actors',
        metadata => #{
            links => #{'ca.a.test.my_actors' => my_link},
            labels => #{
                is_a => true,
                is_b => true
            },
            fts => #{fts_class => <<"Domáin b"/utf8>>}
        }
    },
    D3= #{
        resource => configmaps,
        name => cc,
        namespace => 'c.b.a.test.my_actors',
        metadata => #{
            links => #{'cb.b.a.test.my_actors' => my_link},
            labels => #{
                is_a => true,
                is_b => true,
                is_c => true
            },
            fts => #{fts_class => <<"Domáin c"/utf8>>}
        }
    },
    U1 = #{
        resource => users,
        name => ut1,
        namespace => 'b.a.test.my_actors',
        data => #{spec => #{password => pass1}},
        metadata=> #{
            links => #{'cb.b.a.test.my_actors' => my_link},
            labels => #{
                is_a => true,
                is_b => true
            },
            fts => #{fts_name => <<"Úser MY name"/utf8>>}
        }
    },
    #{d1=>D1, d2=>D2, d3=>D3, u1=>U1}.




req(Req) ->
    Base = #{
        srv => ?ACTOR_SRV,          % We will update later if different
        group => ?GROUP_CORE,
        auth => #{token=>?TOKEN}
        %namespace => ?NAMESPACE
    },
    Req2 = maps:merge(Base, Req),
    {Action, Data, _Req2} = nkactor_request:request(Req2),
    {Action, Data}.


kapi_req(Req) ->
    Base = #{
        srv => ?ACTOR_SRV,          % We will update later if different
        group => ?GROUP_CORE,
        vsn => <<"v1a1">>,
        auth => #{token=>?TOKEN}
    },
    Req2 = maps:merge(Base, Req),
    {Action, Data, _Req2} = nkactor_kapi:request(Req2),
    {Action, Data}.



api_watch(Api) ->
    Ref = make_ref(),
    Api2 = Api#{verb=>watch, callback=>?MODULE, meta => #{nkdomain_api_pid=>self(), nkdomain_api_ref=>Ref}},
    Pid = spawn_link(fun() -> req(Api2) end),
    {Ref, Pid}.


api_watch_stop({_Ref, Pid}) ->
    Pid ! stop_watch.


%% @private
wait_api_event({Ref, _Pid}=Id, Reason) ->
    receive
        {api_event, Ref, Body} ->
            self() ! {api_event2, Ref, Body},
            % io:format("API EVENT ~p\n", [Body]),
            wait_api_event(Id, Reason);
        {api_event2, Ref, #{<<"type">>:=Type, <<"object">>:=Ev}} when Reason == <<>> ->
            {Type, Ev};
        {api_event2, Ref, #{<<"type">>:=Type, <<"object">>:=#{<<"reason">>:=Reason}=Ev}} ->
            {Type, Ev}
    after 1000 ->
        api_event_timeout
    end.


%% @private
clean_events() ->
    receive
        Msg -> {error, Msg}
    after 1500 ->
        no_message
    end.



%% @doc
new_event(Body, #{meta:=#{nkdomain_api_pid:=Pid, nkdomain_api_ref:=Ref}}=ApiReq) ->
    Pid ! {api_event, Ref, Body},
    {ok, ApiReq}.


http_get(Path) ->
    {ok, {{_, Code, _}, _Hds, Body}} = httpc:request(get, {http_url(Path), http_auth_header()}, [], []),
    {Code, nklib_json:decode(Body)}.

http_post(Path, Body) ->
    Body2 = nklib_json:encode(Body),
    {ok, {{_, Code, _}, _Hds, Body3}} = httpc:request(post, {http_url(Path), http_auth_header(), "application/json", Body2}, [], []),
    {Code, nklib_json:decode(Body3)}.

http_put(Path, Body) ->
    Body2 = nklib_json:encode(Body),
    {ok, {{_, Code, _}, _Hds, Body3}} = httpc:request(put, {http_url(Path), http_auth_header(), "application/json", Body2}, [], []),
    {Code, nklib_json:decode(Body3)}.

http_delete(Path) ->
    {ok, {{_, Code, _}, _Hds, Body}} = httpc:request(delete, {http_url(Path), http_auth_header(), "", ""}, [], []),
    {Code, nklib_json:decode(Body)}.

http_list(Path) ->
    Path2 = case lists:member($?, Path) of
        true ->
            Path ++ "&getTotals=true";
        false ->
            Path ++ "?getTotals=true"
    end,
    {200, List} =  http_get(Path2),
    #{<<"metadata">> := #{<<"total">>:=Total, <<"size">>:=Size}, <<"items">> := Items} = List,
    {Total, Size, Items}.

http_search(Namespace, Spec) ->
    Path = binary_to_list(list_to_binary([http_host(), "/search/v1a1/namespaces/" ++ Namespace])),
    Spec2 = Spec#{
        get_totals => true,
        get_data => true,
        get_metadata => true
    },
    Body = nklib_json:encode(Spec2),
    case httpc:request(post, {Path, http_auth_header(), "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _Hds, List}} ->
            #{<<"items">>:=Items, <<"total">>:=Total, <<"size">>:=Size} = nklib_json:decode(List),
            {Total, Size, Items};
        {ok, {{_, Code, _}, _Hds, Body2}} ->
            {Code, nklib_json:decode(Body2)}
    end.


http_search_delete(Namespace, Spec) ->
    Path = binary_to_list(list_to_binary([http_host(), "/search/v1a1/namespaces/" ++ Namespace ])),
    Body = nklib_json:encode(Spec),
    {ok, {{_, Code, _}, _Hds, Body2}} = httpc:request(delete, {Path, http_auth_header(), "application/json", Body}, [], []),
    {Code, nklib_json:decode(Body2)}.


http_host() ->
    ?LISTEN.


http_url(Path) ->
    Bin = list_to_binary([http_host(), "/apis/core/v1a1", Path]),
    binary_to_list(Bin).



http_watch(Path) ->
    Url = "http://127.0.0.1:9001/apis/core/v1a1" ++ Path ++ "?watch=true",
    % lager:error("NKLOG URL ~p", [Url]),
    Opts = [{connect_timeout, 1000}, {recv_timeout, 20000}, async, with_body],
    {ok, Ref} = hackney:request(get, Url, http_auth_header(), [], Opts),
    receive
        {hackney_response, Ref, {status, 200, _}} ->
            Ref
    after 5000 ->
        error(http_timeout)
    end.


http_watch_stop(Ref) ->
    ok = hackney_manager:close_request(Ref).



http_auth_header() ->
    [{"x-nkdomain-token", nklib_util:to_list(?TOKEN)}].



httpc(Path) ->
    httpc:request(get, {http_url(Path), http_auth_header()}, [], []).

httpc(Method, Path, CT, Body) ->
    httpc:request(Method, {http_url(Path), http_auth_header(), CT, Body}, [], []).


%% @private
wait_http_event(Ref, Reason) ->
    wait_http_event(Ref, Reason, <<>>).


%% @private
wait_http_event(Ref, Reason, Buff) ->
    receive
        {hackney_response, Ref, {headers, Headers}} ->
            #{
                <<"Content-Type">> := <<"application/json">>,
                <<"transfer-encoding">> := <<"chunked">>
            } = maps:from_list(Headers),
            wait_http_event(Ref, Reason, Buff);
        {hackney_response, Ref, <<"\r\n">>} ->
            wait_http_event(Ref, Reason, <<>>);
        {hackney_response, Ref, Body} ->
            Body2 = <<Buff/binary, Body/binary>>,
            case catch nklib_json:decode(Body2) of
                {'EXIT', _} ->
                    % lager:error("NKLOG MORE DATA ~p", [Body2]),
                    wait_http_event(Ref, Reason, Body2);
                Event ->
                    self() ! {http_event, Ref, Event},
                    % io:format("HTTP EVENT ~s\n", [nklib_json:encode_pretty(Event)]),
                    wait_http_event(Ref, Reason, <<>>)
            end;
        {http, Ref, done} ->
            stream_end;
        {http, Ref, {error, Error}} ->
            lager:error("NKLOG Hackney error: ~p", [Error]),
            stream_end;
        stop ->
            stop;
        {http_event, Ref, #{<<"type">>:=Type, <<"object">>:=Ev}} when Reason == <<>> ->
            {Type, Ev};
        {http_event, Ref, #{<<"type">>:=Type, <<"object">>:=#{<<"reason">>:=Reason}=Ev}} ->
            {Type, Ev}
    after 1000 ->
        http_event_timeout
    end.



k8s_watch() ->
     Url = "http://127.0.0.1:8001/api/v1/events?watch=true",
    {ok, Ref} = httpc:request(get, {Url, []}, http_auth_header(), [{sync, false}, {stream, self}]),
    receive
        {http, {Ref, event_stream_start, Hds}} ->
            #{
                "content-type" := "application/json",
                "transfer-encoding" := "chunked"
            } = maps:from_list(Hds),
            Events = k8s_watch_events(Ref, 5000, []),
            catch httpc:cancel_request(Ref),
            {ok, Events}
    after 5000 ->
        {error, timeout}
    end.


k8s_watch_events(Ref, Chunks, Acc) when Chunks > 0 ->
    receive
        {http, {Ref, stream, Body}} ->
            Event = (catch nklib_json:decode(Body)),
            lager:error("NKLOG STREAMK: ~p", [Event]),
            k8s_watch_events(Ref, Chunks-1, [Event|Acc]);
        {http, {Ref, stream_end, _Hds}} ->
            lager:error("NKLOG END"),
            Acc;
        Other ->
            lager:error("NKLOG HTTPC OTHER ~p", [Other]),
            {error, {httpc_closed, Other}}
    after 600000 ->
        error(600000)
    end;

k8s_watch_events(_Ref, _Chunks, Acc) ->
    Acc.



yaml(Str) ->
    %lager:error("NKLOG STR ~s", [Str]),
    [Obj] = nklib_yaml:decode(Str),
    Obj.




wait(Path) ->
    spawn(
        fun() ->
            R = http_watch(Path++"?watch=true"),
            lager:error("Http Watch result: ~p", [R])
        end).


send_test_event() ->
    send_test_event("/nkdomain-root/core/domain/root").


send_test_event(Path) ->
    nkactor:async_op(Path, {send_event, test_api}).




w(Vsn) ->
    nkdomain_api_events:wait_for_save(),
    api_watch(#{resource=>domains, name=>root, params=>#{deep=>false, resourceVersion=>Vsn}}),
    timer:sleep(500),
    lager:error("NKLOG SENDING1"),
    send_test_event().



get_linked_uids(Type, #{<<"metadata">>:=#{<<"links">>:=Links}}) ->
    maps:fold(
        fun(UID, FunType, Acc) ->
            case Type==FunType of
                true -> [UID|Acc];
                false -> Acc
            end
        end,
        [],
        Links).



%%% Delete "TestAPI" events at root
%%delete_root_test_api() ->
%%    Opts = #{
%%        filter => #{
%%            'and' => [
%%                #{field => <<"type">>, value => <<"event">>},
%%                #{field => <<"reason">>, value=><<"TestAPI">>}
%%            ]
%%        }
%%    },
%%    nkservice_actor:delete_all(?DOMAIN_SRV_ROOT, Opts#{delete=>true}, #{}).


