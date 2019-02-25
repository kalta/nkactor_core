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

%% @doc Default plugin callbacks
-module(nkactor_core_api_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([api_get_groups/2, api_get_modules/3, api_request/3]).

-include("nkactor_core.hrl").

%% ===================================================================
%% Callbacks
%% ===================================================================


%% @doc
api_get_groups(SrvId, GroupsAcc) ->
    GroupsAcc2 = GroupsAcc#{?GROUP_CORE => [?GROUP_CORE_API_V1A1]},
    {continue, [SrvId, GroupsAcc2]}.


%% @doc
api_get_modules(_SrvId, ?GROUP_CORE, ?GROUP_CORE_API_V1A1) ->
    {ok, nkactor_core_app:modules()};

api_get_modules(_SrvId, ?GROUP_CORE, Vsn) ->
    {error, {api_incompatible, <<"core/", Vsn/binary>>}};

api_get_modules(_SrvId, _Group, _Vsn) ->
    continue.


%% @doc
api_request(SrvId, <<"core">>, #{vsn:=Vsn}=ApiReq) ->
    case Vsn of
        ?GROUP_CORE_API_V1A1 ->
            nkactor_core_api_request:request(SrvId, Vsn, ApiReq);
        _ ->
            {error, {api_incompatible, <<"core/", Vsn/binary>>}}
    end;

api_request(SrvId, <<"search">>, #{vsn:=?GROUP_CORE_API_V1A1}=ApiReq) ->
    nkactor_core_api_search:search(SrvId, ApiReq);

api_request(_SrvId, _Group, _ApiReq) ->
    continue.

