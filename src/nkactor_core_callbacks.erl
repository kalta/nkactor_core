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


msg({actor_invalid, A})                 -> {"Invalid actor '~s'", [A]};
msg({api_group_unknown, G})             -> {"Unknown API Group '~s'", [G]};
msg({api_incompatible, A})              -> {"Incompatible API '~s'", [A]};
msg({api_unknown, A})                   -> {"Unknown API '~s'", [A]};
msg({body_too_large, Max})              -> {"Body too large (max is ~p)", [Max]};
msg({could_not_load_parent, Id})        -> {"Object could not load parent '~s'", [Id]};
msg({could_not_load_domain, Id})        -> {"Object could not load domain '~s'", [Id]};
msg({could_not_load_user, Id})          -> {"Object could not load user '~s'", [Id]};
msg(content_type_invalid)               -> "ContentType is invalid";
msg(db_not_defined)                     -> "Object database not defined";
msg({domain_unknown, D})                -> {"Unknown domain '~s'", [D]};
msg(domain_invalid)                     -> "Invalid domain";
msg({domain_is_disabled, D})            -> {"Domain '~s' is disabled", [D]};
msg(domains_name_cannot_change)         -> "ObjName cannot be updated for domains";
msg(download_server_error)              -> "Download server error";
msg({email_duplicated, E})              -> {"Duplicated email '~s'", [E]};
msg(element_action_unknown)             -> "Unknown element action";
msg({file_not_found, F})                -> {"File '~s' not found", [F]};
msg(file_is_invalid)                    -> "File is invalid";
msg(file_too_large)                     -> "File is too large";
msg(group_unknown)                      -> "Invalid Group";
msg(invalid_content_type)               -> "Invalid Content-Type";
msg({invalid_name, N})                  -> {"Invalid name '~s'", [N]};
msg(invalid_sessionn)                   -> "Invalid session";
msg(kind_unknown)                       -> "Invalid kind";
msg({kind_unknown, K})                  -> {"Invalid kind '~s'", [K]};
msg(member_already_present)             -> "Member is already present";
msg(member_not_found)                   -> "Member not found";
msg(member_invalid)                     -> "Invalid member";
msg(multiple_ids)                       -> "Multiple matching ids";
msg(missing_auth_header)                -> "Missing authentication header";
msg({module_failed, Module})            -> {"Module '~s' failed", [Module]};
msg(object_access_not_allowed)          -> "Object access is not allowed";
msg(object_already_exists)              -> "Object already exists";
msg({object_already_exists, ObjIdOrP})  -> {"Object already exists: ~s", [ObjIdOrP]};
msg(object_clean_process)               -> "Object cleaned (process stopped)";
msg(object_clean_expire)                -> "Object cleaned (expired)";
msg(object_consumed)                    -> "Object is consumed";
msg({object_consumed, R})               -> {"Object is consumed: ~s", [R]};
msg(object_deleted) 		            -> "Object removed";
msg(object_expired) 		            -> "Object expired";
msg(object_has_childs) 		            -> "Object has childs";
msg({object_load_error, Error}) 		-> {"Object load error: '~p'", [Error]};
msg(object_is_already_loaded)           -> "Object is already loaded";
msg(object_is_disabled) 		        -> "Object is disabled";
msg(object_is_stopped) 		            -> "Object is stopped";
msg(object_not_found) 		            -> "Object not found";
msg(object_not_started) 		        -> "Object is not started";
msg(object_path_invalid)                -> "Invalid object path";
msg({object_path_invalid, P})           -> {"Invalid object path '~s'", [P]};
msg(object_parent_conflict) 	        -> "Object has conflicting parent";
msg(object_stopped) 		            -> "Object stopped";
msg(operation_invalid) 	                -> "Invalid operation";
msg(operation_token_invalid) 	        -> "Operation token is invalid";
msg({parameter_invalid, Txt})      	    -> {"Invalid parameter '~s'", [Txt]};
msg({parameter_missing, Txt})      	    -> {"Missing parameter '~s'", [Txt]};
msg(parent_is_disabled) 		        -> "Parent is disabled";
msg(parent_not_found) 		            -> "Parent not found";
msg(parent_stopped) 		            -> "Parent stopped";
msg(parse_error)   		                -> "Object parse error";
msg(password_valid)                     -> "Password is valid";
msg(password_invalid) 	                -> "Password is not valid";
msg(provider_class_unknown)             -> "Provider class is unknown";
msg(request_body_invalid)               -> "The request body is invalid";
msg(resource_invalid)                   -> "Invalid resource";
msg({resource_invalid, R})              -> {"Invalid resource '~s'", [R]};
msg({resource_invalid, G, R})           -> {"Invalid resource '~s' (~s)", [R, G]};
msg(service_down)                       -> "Service is down";
msg(session_already_present)            -> "Session is already present";
msg(session_not_found)                  -> "Session not found";
msg(session_is_disabled)                -> "Session is disabled";
msg({srv_id_invalid, S})                -> {"Invalid service ~s", [S]};
msg(sso_not_available)                  -> "SSO is not available";
msg(status_not_defined)                 -> "Status is not defined";
msg(store_id_invalid)                   -> "Invalid Store Id";
msg(store_id_missing)                   -> "Missing Store Id";
msg(token_invalid)                      -> "Invalid token";
msg(token_invalid_ttl)                  -> "Invalid token TTL";
msg(token_down)                         -> "Token process is down";
msg(task_max_tries_reached)             -> "Task max tries reached";
msg(task_max_time_reached)              -> "Task max time reached";
msg(uid_not_found)      		        -> "Unknown UID";
msg(upload_server_error)                -> "Upload server error";
msg(url_unknown)      		            -> "Unknown url";
msg(user_is_disabled) 		            -> "User is disabled";
msg(user_unknown)                       -> "Unknown user";
msg({user_unknown, UserId})             -> {"Unknown user '~s", [UserId]};
msg(verb_not_allowed)                   -> "Verb is not allowed";
msg(watch_stop)                         -> "Watch stopped";
msg(_)   		                        -> continue.
