%% -------------------------------------------------------------------
%%
%% Copyright (c) 2020 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkActor ConfigMap Config
-module(nkactor_core_cache_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([config/0, parse/3, init/2, sync_op/3, async_op/2, handle_info/2]).

-include_lib("nkserver/include/nkserver.hrl").
-include_lib("nkactor/include/nkactor.hrl").
-include("nkactor_core.hrl").

-define(DEFAULT_TTL, 60*60*1000).

%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_CACHE,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list, update],
        permanent => true
    }.


%% @doc
parse(_Op, _Actor, _Req) ->
    Spec = #{
        class => binary,
        type => binary,
        target_group => binary,
        target_resource => binary,
        target_uid => binary,
        ttl => pos_integer
    },
    {syntax, <<"v1a1">>, #{spec => Spec}}.


%% @private
init(_, ActorSt) ->
    self() ! nkactor_load,
    {ok, set_ttl(ActorSt#actor_st{run_state=#{ttl_timer=>undefined, data=>#{}}})}.


%% @private
sync_op(nkactor_get_cache, _From, #actor_st{run_state=RunState}=ActorSt) ->
    {reply, {ok, maps:get(data, RunState)}, ActorSt};

sync_op(_, _, _) ->
    continue.


%% @private
async_op({nkactor_update, Update}, ActorSt) ->
    #actor_st{srv=SrvId, actor=Actor, run_state=#{data:=Data}=RunState} = ActorSt,
    case ?CALL_SRV(SrvId, actor_core_cache_update, [SrvId, Update, Data, Actor]) of
        {ok, Data2} ->
            {noreply, ActorSt#{run_state:=RunState#{data:=Data2}}};
        {error, Error} ->
            {stop, Error, ActorSt}
    end;

async_op(_, _) ->
    continue.


%% @private
handle_info(nkactor_load, #actor_st{srv=SrvId, run_state=RunState, actor=Actor}=ActorSt) ->
    case ?CALL_SRV(SrvId, actor_core_cache_load, [SrvId, Actor]) of
        {ok, Data2} ->
            {noreply, ActorSt#{run_state:=RunState#{data:=Data2}}};
        {error, Error} ->
            {stop, Error, ActorSt}
    end;

handle_info(nkactor_expired, ActorSt) ->
    {stop, normal, ActorSt};

handle_info(_, _) ->
    continue.


%% ===================================================================
%% Internal
%% ===================================================================



set_ttl(ActorSt) ->
    #actor_st{actor=#{spec:=Spec}, run_state=#{ttl_timer:=Timer}=RunState} = ActorSt,
    nklib_util:cancel_timer(Timer),
    TTL = maps:get(ttl, Spec, ?DEFAULT_TTL),
    Timer2 = erlang:send_after(TTL, self(), nkactor_expired),
    ActorSt#actor_st{run_state=RunState#{ttl_timer:=Timer2}}.


