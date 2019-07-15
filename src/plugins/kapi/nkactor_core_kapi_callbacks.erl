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
-export([actor_to_external/5]).
-include("nkactor_core.hrl").

%% ===================================================================
%% Callbacks
%% ===================================================================


%% @doc
api_get_groups(SrvId, GroupsAcc) ->
    GroupsAcc2 = GroupsAcc#{?GROUP_CORE => [?GROUP_CORE_API_V1A1]},
    {continue, [SrvId, GroupsAcc2]}.



%% @doc
actor_to_external(Class, ?GROUP_CORE, ?RES_CORE_CONTACTS, Actor, Req)
        when Class == nkactor_kapi; Class == nkactor_ksearch ->
    Syntax = #{
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
    },
    {ok, Parsed} = nklib_syntax:parse_all(Actor, Syntax),
    {continue, [Class, ?GROUP_CORE, ?RES_CORE_CONTACTS, Parsed, Req]};

actor_to_external(Class, ?GROUP_CORE, ?RES_CORE_USERS, Actor, Req) ->
    case Actor of
        #{data:=#{<<"spec">>:=#{<<"password">>:=_}=Spec}=Data} ->
            Actor2 = Actor#{data:=Data#{<<"spec">>:=Spec#{<<"password">>:=<<>>}}},
            {continue, [Class, ?GROUP_CORE, ?RES_CORE_USERS, Actor2, Req]};
        _ ->
            continue
    end;

actor_to_external(_Class, _Group, _Res, _Actor, _Req) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================
