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

%% @doc NkActor Contact Actor
-module(nkactor_core_contact_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([config/0, parse/3, init/2, update/3]).

-include("nkactor_core.hrl").
-include_lib("nkactor/include/nkactor.hrl").



%% ===================================================================
%% Actor behaviour
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_CONTACTS,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list, update],
        short_names => [ct],
        fields_filter => [
            'spec.name',
            'spec.surname',
            'spec.birth_time',
            'spec.gender',
            'spec.timezone',
            'spec.phone',
            'status.normalized_name',
            'status.normalized_surname'
        ],
        fields_sort => [
            'spec.gender',
            'spec.birth_time',
            'spec.timezone',
            'status.normalized_name',
            'status.normalized_surname'
        ],
        fields_type => #{
            'spec.timezone' => integer
        }
    }.


%% @doc
%% Valid for normal and CamelCase
parse(Op, Actor, _Req) ->
    Syntax = #{
        spec => #{
            name => binary,
            surname => binary,
            birth_time => date_3339,
            gender => {binary, [<<"M">>, <<"F">>]},
            timezone => binary,
            url => {list, #{
                url => binary,
                type => binary,
                meta => map,
                '__mandatory' => [url]
            }},
            phone => {list, #{
                phone => binary,
                type => binary,
                meta => map,
                '__mandatory' => [phone]
            }},
            email => {list, #{
                email => binary,
                type => binary,
                meta => map,
                '__mandatory' => [email]
            }},
            im => {list, #{
                im => binary,
                type => binary,
                meta => map,
                '__mandatory' => [im]
            }},
            address => {list, #{
                type => binary,
                street => binary,
                code => binary,
                city => binary,
                province => binary,
                state => binary,
                country => binary,
                meta => map
            }},
            pubkey => {list, #{
                key => binary,
                type => binary,
                meta => map,
                '__mandatory' => [key]
            }},
            profile => {list, #{
                type => binary,
                start_time => date_3339,
                stop_time => date_3339,
                data => map,
                meta => map,
                '__mandatory' => [data]
            }},
            photo => {list, #{
                type => binary,
                file => binary,
                meta => map,
                '__mandatory' => [file]
            }},
            user => binary
        },
        status => #{
            normalized_name => binary,
            normalized_surname => binary
        }
    },
    case nkactor_lib:parse_actor_data(Op, Actor, <<"v1a1">>, Syntax) of
        {ok, Actor2} ->
            Data = maps:get(data, Actor2, #{}),
            Spec = maps:get(spec, Data, #{}),
            Name = maps:get(name, Spec, <<>>),
            SurName = maps:get(surname, Spec, <<>>),
            Status1 = maps:get(status, Data, #{}),
            Status2 = Status1#{
                normalized_name => nklib_parse:normalize(Name),
                normalized_surname => nklib_parse:normalize(SurName)
            },
            Actor3 = Actor2#{data=>Data#{status=>Status2}},
            {ok, Actor3};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
init(create, #actor_st{actor=Actor}=ActorSt) ->
    case add_user_link(Actor) of
        {ok, Actor2} ->
            {ok, ActorSt#actor_st{actor=Actor2}};
        {error, Error} ->
            {error, Error}
    end;

init(start, ActorSt) ->
    {ok, ActorSt}.


%% @doc
update(Actor, _Opts, ActorSt) ->
    case add_user_link(Actor) of
        {ok, Actor2} ->
            {ok, Actor2, ActorSt};
        {error, Error} ->
            {error, Error, ActorSt}
    end.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
add_user_link(Actor) ->
    Actor2 = nkactor_lib:rm_links(Actor, ?LINK_CORE_CONTACT_USER),
    case Actor2 of
        #{data:=#{spec:=#{user:=UserId}}} ->
            case nkactor_lib:add_checked_link(UserId, ?GROUP_CORE, ?RES_CORE_USERS, Actor2, ?LINK_CORE_CONTACT_USER) of
                {ok, _, Actor3} ->
                    {ok, Actor3};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {ok, Actor2}
    end.


%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).
