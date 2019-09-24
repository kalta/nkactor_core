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

%% @doc NkActor Token Actor
-module(nkactor_core_token_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([config/0, parse/3, sync_op/3, init/2, expired/2, request/4]).


-include_lib("nkactor/include/nkactor.hrl").
-include("nkactor_core.hrl").


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================


%% @doc
config() ->
    #{
        resource => ?RES_CORE_TOKENS,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list, update],
        ttl => 60 * 60 * 1000
    }.


%% @doc
parse(Op, Actor, Req) ->
    Syntax = #{data => map},
    case nkactor_lib:parse_actor_data(Op, Actor, <<"v1a1">>, Syntax) of
        {ok, #{metadata:=Meta2}=Actor2} ->
            case maps:is_key(expire_time, Meta2) of
                true ->
                    {ok, Actor2};
                false ->
                    case Req of
                        #{params:=#{ttl:=TTL}} when is_integer(TTL), TTL>0 ->
                            % If no expire_time, we use the TTL to generate one
                            Actor3 = nkactor_lib:maybe_set_ttl(Actor2, TTL),
                            {ok, Actor3};
                        _ ->
                            {error, ttl_missing}
                    end
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
request(get, <<"_execute">>, ActorId, Req) ->
    Params = maps:get(params, Req, #{}),
    case nkactor:sync_op(ActorId, {token_execute, Params}) of
        {ok, Reply} ->
            {status, Reply};
        {error, Error} ->
            {error, Error}
    end;

request(_Verb, _Path, _ActorId, _Req) ->
    continue.


%% @doc
init(_Op, #actor_st{actor=#{metadata:=#{expire_time:=Expire}=Meta}}=ActorSt) ->
    % Set the activation time to the expiration date, if not set
    ActorSt2 = case Meta of
        #{activate_time:=_} ->
            ActorSt;
        _ ->
            nkactor_srv_lib:set_activate_time(Expire, ActorSt)
    end,
    {ok, ActorSt2}.


%% @doc
expired(_T, ActorSt) ->
    {delete, ActorSt}.


%% @doc
sync_op({token_execute, _Params}, _From, ActorSt) ->
    % actor_srv_sync_op/3 must be implemented in callback module
    {reply, {error, not_implemented}, ActorSt};

sync_op(_Op, _From, _ActorSt) ->
    continue.


