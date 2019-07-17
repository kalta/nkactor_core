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
-module(nkactor_core_kapi_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([api_get_groups/2]).
-export([actor_kapi_fields_trans/1, actor_kapi_parse/2,
         actor_kapi_unparse/2, actor_kapi_pre_request/5]).
-include("nkactor_core.hrl").

%% ===================================================================
%% Callbacks
%% ===================================================================


%% @doc
api_get_groups(SrvId, GroupsAcc) ->
    GroupsAcc2 = GroupsAcc#{?GROUP_CORE => [?GROUP_CORE_API_V1A1]},
    {continue, [SrvId, GroupsAcc2]}.



%% @doc
actor_kapi_fields_trans(Map) ->
    Map2 = Map#{
        % Contacts
        'data.spec.birthTime' => 'data.spec.birth_time',
        'data.spec.profile.startTime' => 'data.spec.profile.start_time',
        'data.spec.profile.stopTime' => 'data.spec.profile.stop_time',
        'data.status.normalizedName' => 'data.status.normalized_name',
        'data.status.normalizedSurname' => 'data.status.normalized_surname',
        % Tasks
        'data.spec.maxTries' => 'data.spec.max_tries',
        'data.spec.maxSecs' => 'data.spec.max_secs',
        'data.status.taskStatus' => 'data.status.task_status',
        'data.status.lastTryStartTIme' => 'data.status.last_try_start_time',
        'data.status.lastStatusTime' => 'data.status.last_status_time',
        'data.status.errorMsg' => 'data.status.error_msg',
        % Tasks RPC
        'taskStatus' => 'task_status',
        'lastTryStartTIme' => 'last_try_start_time',
        'lastStatusTime' => 'last_status_time',
        'errorMsg' => 'error_msg'
    },
    {continue, [Map2]}.



%% @doc
actor_kapi_pre_request(update, ?GROUP_CORE, ?RES_CORE_TASKS, <<"_state">>, #{body:=Actor}=Req)
    when is_map(Actor)->
    Syntax = #{
        taskStatus => {'__key', task_status},
        lastTryStartTime => {'__key', last_try_start_time},
        lastStatusTime => {'__key', last_status_time},
        errorMsg => {'__key', error_msg}
    },
    {ok, Parsed} = nklib_syntax:parse_all(Actor, Syntax),
    {continue, [?GROUP_CORE, ?RES_CORE_TASKS, <<"_state">>, Req#{body:=Parsed}]};

actor_kapi_pre_request(_Verb, _Group, _Res, _SubRes, _Req) ->
    continue.


%% @doc
actor_kapi_parse(?GROUP_CORE, ?RES_CORE_CONTACTS) ->
    #{
        data => #{
            spec => #{
                birthTime => {'__key', birth_time},
                profile => {list, #{
                    startTime => {'__key', start_time},
                    stopTime => {'__key', stop_time}
                }}
            }
        }
    };

actor_kapi_parse(?GROUP_CORE, ?RES_CORE_TASKS) ->
    #{
        data => #{
            spec => #{
                maxTries => {'__key', max_tries},
                maxSecs => {'__key', max_secs}
            },
            status => #{
                taskStatus => {'__key', task_status},
                lastTryStartTime => {'__key', last_try_start_time},
                lastStatusTime => {'__key', last_status_time},
                errorMsg => {'__key', error_msg}
            }
        }
    };

actor_kapi_parse(_Group, _Res) ->
    continue.


%% @doc
actor_kapi_unparse(?GROUP_CORE, ?RES_CORE_USERS) ->
    #{
        data => #{
            spec => {'__key', <<"spec">>, #{
                password => {'__key', <<"password">>, {force, <<>>}}
            }}
        }
    };

actor_kapi_unparse(?GROUP_CORE, ?RES_CORE_CONTACTS) ->
    #{
        data => #{
            spec => {'__key', <<"spec">>, #{
                name => {'__key', <<"name">>},
                surname => {'__key', <<"surname">>},
                birth_time => {'__key', <<"birthTime">>},
                gender => {'__key', <<"gender">>},
                timezone => {'__key', <<"timezone">>},
                url => {'__key', <<"url">>, {list, #{
                    url => {'__key', <<"url">>},
                    type => {'__key', <<"type">>},
                    meta => {'__key', <<"meta">>}
                }}},
                phone => {'__key', <<"phone">>, {list, #{
                    phone => {'__key', <<"phone">>},
                    type => {'__key', <<"type">>},
                    meta => {'__key', <<"meta">>}
                }}},
                email => {'__key', <<"email">>, {list, #{
                    email => {'__key', <<"email">>},
                    type => {'__key', <<"type">>},
                    meta => {'__key', <<"meta">>}
                }}},
                im => {'__key', <<"im">>, {list, #{
                    im => {'__key', <<"im">>},
                    type => {'__key', <<"type">>},
                    meta => {'__key', <<"meta">>}
                }}},
                address => {'__key', <<"address">>, {list, #{
                    type => {'__key', <<"type">>},
                    street => {'__key', <<"street">>},
                    code => {'__key', <<"code">>},
                    city => {'__key', <<"city">>},
                    province => {'__key', <<"province">>},
                    state => {'__key', <<"state">>},
                    country => {'__key', <<"country">>},
                    meta => {'__key', <<"meta">>}
                }}},
                pubkey => {'__key', <<"pubkey">>, {list, #{
                    key => {'__key', <<"key">>},
                    type => {'__key', <<"type">>},
                    meta => {'__key', <<"meta">>}
                }}},
                profile => {'__key', <<"profile">>, {list, #{
                    type => {'__key', <<"type">>},
                    start_time => {'__key', <<"startTime">>},
                    stop_time => {'__key', <<"stopTime">>},
                    data => {'__key', <<"data">>},
                    meta => {'__key', <<"meta">>}
                }}},
                photo => {'__key', <<"photo">>, {list, #{
                    type => {'__key', <<"type">>},
                    file => {'__key', <<"file">>},
                    meta => {'__key', <<"meta">>}
                }}},
                user => {'__key', <<"user">>}
            }},
            status => {'__key', <<"status">>, #{
                % It is always calculated
                normalized_name => {'__key', <<"normalizedName">>},
                normalized_surname => {'__key', <<"normalizedSurname">>}
            }}
        }
    };

actor_kapi_unparse(?GROUP_CORE, ?RES_CORE_TASKS) ->
    #{
        data => #{
            spec => {'__key', <<"spec">>, #{
                job => {'__key', <<"job">>},
                max_tries => {'__key', <<"maxTries">>},
                max_secs => {'__key', <<"maxSecs">>}
            }},
            status => {'__key', <<"status">>, #{
                task_status => {'__key', <<"taskStatus">>, binary},
                tries => {'__key', <<"tries">>},
                last_try_start_time => {'__key', <<"lastTryStartTime">>},
                last_status_time => {'__key', <<"lastStatusTime">>},
                progress => {'__key', <<"progress">>},
                error_msg => {'__key', <<"errorMsg">>}
            }}
        }
    };

actor_kapi_unparse(_Group, _Res) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================
