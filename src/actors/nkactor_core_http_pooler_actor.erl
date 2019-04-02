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

%% @doc NkActor HTTP Pooler Actor
-module(nkactor_core_http_pooler_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behavior(nkactor_actor).

-export([config/0, parse/2]).
-export([init/2, delete/1]).

-include_lib("nkactor/include/nkactor.hrl").
-include_lib("nkactor/include/nkactor_debug.hrl").
-include("nkactor_core.hrl").

-define(POOL_MAX_CONNECTIONS, 10).
-define(POOL_TIMEOUT, 30*60*1000).


%% ===================================================================
%% Behavior callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        versions => [<<"0">>],
        resource => ?RES_CORE_HTTP_POOOLER,
        camel => <<"HttpPooler">>,
        verbs => [create, delete, deletecollection, get, list, patch, update, watch]
    }.


%% @doc
parse(_Actor, _Req) ->
    Syntax = #{
        spec => #{
            id => binary,
            max_connections => {integer, 1, 1000000},
            timeout => {integer, 1, none},
            '__mandatory' => [id]
        },
        '__mandatory' => [spec]
    },
    {syntax, Syntax}.




%% ===================================================================
%% Actor callbacks
%% ===================================================================

%% @private
init(_Op, #actor_st{actor=#{spec:=#{id:=Id}=Spec}}=ActorSt) ->
    Max = maps:get(max_connections, Spec, ?POOL_MAX_CONNECTIONS),
    Timeout = maps:get(timeout, Spec, ?POOL_TIMEOUT),
    ok = hackney_pool:start_pool(Id, []),
    ok = hackney_pool:set_max_connections(Id, Max),
    ok = hackney_pool:set_timeout(Id, Timeout),
    Max = hackney_pool:max_connections(Id),
    Timeout = hackney_pool:timeout(Id),
    ?ACTOR_LOG(notice, "started Hackney Pool ~s (~p, ~p)", [Id, Max, Timeout]),
    {ok, ActorSt}.


delete(#actor_st{actor=#{spec:=#{id:=Id}}}=ActorSt) ->
    hackney_pool:stop_pool(Id),
    {ok, ActorSt}.


