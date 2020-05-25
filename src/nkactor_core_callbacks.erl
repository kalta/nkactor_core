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

%% @doc Default plugin callbacks
-module(nkactor_core_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([status/1]).
-export([actor_core_cronjobs_activate/4, actor_core_events_saved/2]).
-export([actor_core_cache_load/2, actor_core_cache_update/4]).

status(avatar_not_found)                    -> "Avatar is not found";
status({email_duplicated, E})              -> {"Duplicated email '~s'", [E]};
status({file_not_found, F})                -> {"File '~s' not found", [F]};
status(file_is_invalid)                    -> "File is invalid";
status(file_too_large)                     -> "File is too large";
status({label_not_found, L})               -> {"Label not found: '~s'", [L]};
status({login_exists, L})                  -> {"Login '~s' already exists", [L]};
status({login_unknown, L})                 -> {"Login unknown: '~s'", [L]};
status(provider_class_unknown)             -> "Provider class is unknown";
status(token_invalid)                      -> "Invalid token";
status(token_invalid_ttl)                  -> "Invalid token TTL";
status(token_down)                         -> "Token process is down";
status(task_max_tries_reached)             -> {"Task max tries reached", #{code=>422}};
status(task_max_time_reached)              -> {"Task max time reached", #{code=>422}};
status(user_is_disabled) 		           -> "User is disabled";
status(user_unknown)                       -> "Unknown user";
status({user_unknown, UserId})             -> {"Unknown user '~s", [UserId]};
status(watch_stop)                         -> "Watch stopped";
status(_)   		                        -> continue.


%% @doc Called when a CronJob activates
%% Spawn as soon as possible to avoid blocking the actor
-spec actor_core_cronjobs_activate(binary()|undefined, binary()|undefined,
                                   nkactor:uid()|undefined, nkactor:actor()) ->
    {ok, nkactor:actor()}.

actor_core_cronjobs_activate(_Class, _Type, _TargetUID, Actor) ->
    {ok, Actor}.


%% @doc Called when a series of events have been saved
-spec actor_core_events_saved(nkserver:id(), [nkactor:actor()]) ->
    ok.

actor_core_events_saved(_SrvId, _Events) ->
    ok.


%% @doc Called when a Cache Actor first loads, must return initial data
-spec actor_core_cache_load(nkserver:id(), nkactor:actor()) ->
    {ok, map()} | {error, Reason::term()} | continue | {continue, list()}.

actor_core_cache_load(_SrvId, _Actor) ->
    {ok, #{}}.


%% @doc Called when a Cache Actor needs to be updated, must return new data
-spec actor_core_cache_update(nkserver:id(), map(), map(), nkactor:actor()) ->
    {ok, map()} | {error, Reason::term()} | continue | {continue, list()}.

actor_core_cache_update(_SrvId, _Update, Data, _Actor) ->
    {ok, Data}.
