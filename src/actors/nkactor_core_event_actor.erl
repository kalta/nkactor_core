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

%% @doc NkActor Event Actor
-module(nkactor_core_event_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([config/0]).

-include("nkactor_core.hrl").


%% ===================================================================
%% Actor behaviour
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_EVENTS,
        versions => [<<"0">>],
        verbs => [delete, deletecollection, get, list, watch],
        short_names => [ev],
        activable => false,
        filter_fields => [
            reason,
            count,
            first_timestamp,
            last_timestamp,
            'obj.uid',
            'obj.namespace',
            'obj.group',
            'obj.resource',
            'obj.name',
            'obj.subtype',
            'obj.hash'
        ],
        sort_fields => [
            reason,
            count,
            first_timestamp,
            last_timestamp,
            'obj.namespace',
            'obj.group',
            'obj.resource',
            'obj.name',
            'obj.subtype'
        ],
        field_type => #{
            count => integer
        }
    }.


