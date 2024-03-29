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

-export([config/0, parse/2, sync_op/3, init/2, request/4, stop/2]).


-include_lib("nkactor/include/nkactor.hrl").
-include("nkactor_core.hrl").


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_SESSIONS,
        versions => [<<"0">>],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch]
    }.


%% @doc
parse(Actor, _ApiReq) ->
    Syntax = #{
        spec => #{
            ttl_secs => pos_integer,
            '__mandatory' => [ttl_secs]
        },
        data => map,
        '__mandatory' => [spec]
    },
    case nkactor_lib:parse_actor_data(Actor, Syntax) of
        {ok, #{data:=Data2, metadata:=Meta2}=Actor2} ->
            #{spec:=#{ttl_secs:=Secs}} = Data2,
            Now = nklib_date:epoch(msecs),
            {ok, Expires} = nklib_date:to_3339(Now+1000*Secs, msecs),
            Meta3 = Meta2#{expires_time => Expires},
            {ok, Actor2#{metadata:=Meta3}};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
request(get, [<<"_rpc">>, <<"refresh">>], ActorId, _Req) ->
    case nkactor:sync_op(ActorId, refresh) of
        ok ->
            {status, actor_updated};
        {error, Error} ->
            {error, Error}
    end;

request(_Verb, _Path, _ActorId, _Req) ->
    continue.


%% @doc
init(_Op, #actor_st{unload_policy = {expires, _}}=ActorSt) ->
    % The parser shouldn't allow to get to this point
    {ok, ActorSt}.


%% @doc
sync_op(refresh, _From, ActorSt) ->
    #actor_st{actor=#{data:=Data, metadata:=Meta}=Actor} = ActorSt,
    #{spec:=#{ttl_secs:=Secs}} = Data,
    Now = nklib_date:epoch(msecs),
    {ok, Expires} = nklib_date:to_3339(Now+1000*Secs, msecs),
    Meta2 = Meta#{expires_time => Expires},
    Actor2 = Actor#{metadata := Meta2},
    {ok, ActorSt2} = nkactor_srv:do_update(Actor2, #{}, ActorSt),
    {reply, ok, ActorSt2};

sync_op(_Op, _From, _ActorSt) ->
    continue.


%% @doc If expires, delete the actor
stop(actor_expired, ActorSt) ->
    {delete, ActorSt};

stop(_Reason, ActorSt) ->
    {ok, ActorSt}.

