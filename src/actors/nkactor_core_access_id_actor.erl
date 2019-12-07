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

%% @doc NkActor Access Id Actor
%% It generates a label for the field 'id'
-module(nkactor_core_access_id_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([find_id/4]).
-export([config/0, parse/3, init/2, update/3]).

-include("nkactor_core.hrl").
-include_lib("nkactor/include/nkactor.hrl").



%% ===================================================================
%% API
%% ===================================================================

-spec find_id(nkserver:id(), nkactor:namespace(), binary(), binary()) ->
    {ok, #actor_id{}} | {error, term()}.

find_id(SrvId, Namespace, Class, Id) ->
    nkactor:find_cached_label(SrvId, Namespace, make_label_key(Class), Id).


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_ACCESS_IDS,
        versions => [<<"v1a1">>],
        camel => <<"AccessId">>,
        verbs => [create, delete, deletecollection, get, list, update]
    }.


%% @doc
parse(_Op, Actor, _Req) ->
    Syntax = #{
        spec => #{
            class => binary,
            id => binary,
            '__mandatory' => [class, id]
        },
        '__mandatory' => spec
    },
    {syntax, <<"v1a1">>, Syntax, Actor}.


%% @doc
init(create, #actor_st{actor=Actor}=ActorSt) ->
    Actor2 = add_label(Actor),
    {ok, ActorSt#actor_st{actor = Actor2}};

init(start, ActorSt) ->
    {ok, ActorSt}.


%% @doc
update(Actor, _Opts, ActorSt) ->
    Actor2 = add_label(Actor),
    {ok, Actor2, ActorSt}.


%% ===================================================================
%% Internal
%% ===================================================================

%% @private
add_label(#{data:=#{spec:=#{class:=Class, id:=Id}}}=Actor) ->
    Actor2 = nkactor_lib:rm_label_re(?LABEL_ACCESS_ID, Actor),
    nkactor_lib:add_label(make_label_key(Class), Id, Actor2).


%% @private
make_label_key(Class) ->
    Class2 = nklib_util:to_binary(Class),
    <<?LABEL_ACCESS_ID/binary, $-, Class2/binary>>.
