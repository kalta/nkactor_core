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
-module(nkactor_core_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([msg/1]).


msg({email_duplicated, E})              -> {"Duplicated email '~s'", [E]};
msg({file_not_found, F})                -> {"File '~s' not found", [F]};
msg(file_is_invalid)                    -> "File is invalid";
msg(file_too_large)                     -> "File is too large";
msg(provider_class_unknown)             -> "Provider class is unknown";
msg(token_invalid)                      -> "Invalid token";
msg(token_invalid_ttl)                  -> "Invalid token TTL";
msg(token_down)                         -> "Token process is down";
msg(task_max_tries_reached)             -> "Task max tries reached";
msg(task_max_time_reached)              -> "Task max time reached";
msg(user_is_disabled) 		            -> "User is disabled";
msg(user_unknown)                       -> "Unknown user";
msg({user_unknown, UserId})             -> {"Unknown user '~s", [UserId]};
msg(watch_stop)                         -> "Watch stopped";
msg(_)   		                        -> continue.
