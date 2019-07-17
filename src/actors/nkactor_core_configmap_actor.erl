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

%% @doc NkActor ConfigMap Config
-module(nkactor_core_configmap_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([config/0, parse/2]).

-include("nkactor_core.hrl").


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_CONFIGMAPS,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch],
        camel => <<"ConfigMap">>
    }.


%% @doc
parse(_Actor, _ApiReq) ->
    {syntax, <<"v1a1">>, #{data => map}}.

