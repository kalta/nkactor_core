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
-module(nkactor_core_access_id_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([find_id/3]).
-export([config/0, parse/3, update/2]).

-include("nkactor_core.hrl").

-define(LABEL_ID, <<"id.netc.io">>).



%% ===================================================================
%% API
%% ===================================================================

find_id(SrvId, Class, Id) ->
    nkactor:find_label(SrvId, make_label_key(Class), Id).


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_ACCESS_IDS,
        versions => [<<"v1a1">>],
        camel => <<"AccessId">>,
        verbs => [create, delete, deletecollection, get, list, update, watch]
    }.


%% @doc
parse(_Verb, Actor, Req) ->
    Syntax = #{
        spec => #{
            class => binary,
            id => binary,
            '__mandatory' => [class, id]
        }
    },
    case nkactor_lib:parse_actor_data(Actor, <<"v1a1">>, Syntax, Req) of
        {ok, Actor2} ->
            {ok, add_label(Actor2)};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
update(Actor, State) ->
    Actor2 = nkactor_lib:rm_label_re(?LABEL_ID, Actor),
    Actor3 = add_label(Actor2),
    {ok, Actor3, State}.




%% ===================================================================
%% Internal
%% ===================================================================

%% @private
add_label(#{data:=#{spec:=#{class:=Class, id:=Id}}}=Actor) ->
    nkactor_lib:add_label(make_label_key(Class), Id, Actor).

%% @private
make_label_key(Class) ->
    <<Class/binary, $., ?LABEL_ID/binary>>.
