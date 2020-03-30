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

%% @doc NkActor Event Actor
-module(nkactor_core_event_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([config/0, parse/3]).

-include("nkactor_core.hrl").


%% ===================================================================
%% Actor behaviour
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_EVENTS,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list],
        short_names => [ev],
        activable => false,
        fields_filter => [
            'class',
            'type',
            'count',
            'first_timestamp',
            'last_timestamp',
            'target.uid',
            'target.group',
            'target.resource',
            'target.name',
            'target.hash'
        ],
        fields_sort => [
            'class',
            'type',
            'first_timestamp',
            'last_timestamp',
            'target.uid',
            'target.group',
            'target.resource',
            'target.name'
        ],
        fields_type => #{
            'count' => integer,
            'tags' => array
        }
    }.



%% @doc
parse(_Op, _Actor, _Req) ->
    Syntax = #{
        class => binary,
        type => binary,
        count => pos_integer,
        message => binary,
        body => map,
        first_timestamp => pos_integer,
        last_timestamp => pos_integer,
        tags => {list, binary},
        target => #{
            uid => binary,
            group => binary,
            resource => binary,
            name => binary,
            hash => binary
        },
        '__mandatory' => [class, type, count, first_timestamp],
        '__defaults' => #{body => #{}, message => <<>>, tags => []}
    },
    {syntax, <<"v1a1">>, Syntax}.
