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

%% @doc Default callbacks for plugin definitions
-module(nkactor_core_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([plugin_deps/0, plugin_config/3]).

-include("nkactor_core.hrl").

%% ===================================================================
%% Plugin Callbacks
%% ===================================================================


%% @doc 
plugin_deps() ->
	[nkactor].


%% @doc
plugin_config(_SrvId, Config, _Service) ->
	Modules = [
		nkactor_core_configmap_actor,
		nkactor_core_contact_actor,
		nkactor_core_http_pooler_actor,
		nkactor_core_event_actor,
		nkactor_core_access_id_actor,
		nkactor_core_node_actor,
		nkactor_core_session_actor,
		nkactor_core_task_actor,
		nkactor_core_token_actor,
		nkactor_core_user_actor
	],
	nkactor_plugin:add_modules(Config, ?GROUP_CORE, Modules).


