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

%% @doc NkActor Session Actor
-module(nkactor_core_session_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([config/0, parse/3, sync_op/3, init/2, request/4, expired/2]).


-include_lib("nkactor/include/nkactor.hrl").
-include("nkactor_core.hrl").


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_SESSIONS,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list, update],
        auto_activate => true
    }.


%% @doc
parse(Op, Actor, _Req) ->
    Syntax = #{
        spec => #{
            ttl_secs => pos_integer,
            '__mandatory' => [ttl_secs]
        },
        data => map,
        '__mandatory' => [spec]
    },
    case nkactor_lib:parse_actor_data(Op, Actor, <<"v1a1">>, Syntax) of
        {ok, #{data:=Data2}=Actor2} ->
            #{spec:=#{ttl_secs:=Secs}} = Data2,
            Actor3 = nkactor_lib:maybe_set_ttl(Actor2, 1000*Secs),
            {ok, Actor3};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
request(get, <<"_rpc/refresh">>, ActorId, _Req) ->
    case nkactor:sync_op(ActorId, nkactor_refresh) of
        ok ->
            {status, actor_updated};
        {error, Error} ->
            {error, Error}
    end;

request(_Verb, _Path, _ActorId, _Req) ->
    continue.


%% @doc
init(_Op, #actor_st{actor=#{metadata:=#{expire_time:=_}}}=ActorSt) ->
    {ok, ActorSt}.


%% @doc
sync_op(nkactor_refresh, _From, ActorSt) ->
    #actor_st{actor=#{data:=Data}=Actor} = ActorSt,
    #{spec:=#{ttl_secs:=Secs}} = Data,
    Actor2 = nkactor_lib:set_ttl(Actor, 1000*Secs),
    ActorSt2 = nkactor_srv_lib:set_times(ActorSt#actor_st{actor=Actor2}),
    {reply, ok, ActorSt2};

sync_op(_Op, _From, _ActorSt) ->
    continue.


%% @doc If expires, delete the actor
expired(_Time, ActorSt) ->
    {delete, ActorSt}.

