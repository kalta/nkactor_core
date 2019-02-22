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

-export([config/0, parse/2]).
-export([parse_post_check/1]).

-include("nkactor_core.hrl").




%% ===================================================================
%% Actor behaviour
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_CONTACTS,
        versions => [<<"0">>],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch],
        short_names => [ct],
        filter_fields => [
            'spec.name',
            'spec.surname',
            'spec.normalized_name',
            'spec.normalized_surname',
            'spec.birth_time',
            'spec.gender',
            'spec.timezone',
            'spec.phone'
        ],
        sort_fields => [
            'spec.normalized_name',
            'spec.normalized_surname',
            'spec.gender',
            'spec.birth_time',
            'spec.timezone'
        ],
        field_type => #{
            'spec.timezone' => integer
        }
    }.


%% @doc
parse(Actor, #{srv:=SrvId}) ->
    Spec = #{
        name => binary,
        surname => binary,
        birth_time => date_3339,
        gender => {binary, [<<"M">>, <<"F">>]},
        timezone => integer,
        url => #{
            '__key_binary' => #{
                type => binary,
                meta => map
        }},
        phone => #{
            '__key_binary' => #{
                type => binary,
                meta => map
        }},
        email => #{
            '__key_binary' => #{
                type => binary,
                meta => map
        }},
        im => #{
            '__key_binary' => #{
                type => binary,
                meta => map
        }},
        address => #{
            '__key_binary' => #{
                type => binary,
                street => binary,
                code => binary,
                city => binary,
                province => binary,
                state => binary,
                country => binary,
                meta => map
        }},
        pubkey => #{
            '__key_binary' => #{
                type => binary,
                key => binary,
                meta => map,
                '__mandatory' => [key]
        }},
        profile => #{
            '__key_binary' => #{
                type => binary,
                start_time => date_3339,
                stop_time => date_3339,
                data => map,
                meta => map,
                '__mandatory' => [data]
        }},
        photo => #{
            '__key_binary' => #{
                type => binary,
                file => binary,
                meta => map,
                '__mandatory' => [file]
        }},
        % It is always calculated
        normalized_name => binary,
        normalized_surname => binary,
        user => binary,
        '__post_check' => fun ?MODULE:parse_post_check/1
    },
    case nkactor_lib:parse_actor_data(Actor, #{spec=>Spec}) of
        {ok, Actor2} ->
            add_user_link(SrvId, Actor2);
        {error, Error} ->
            {error, Error}
    end.




%% ===================================================================
%% Internal
%% ===================================================================


%% @private
add_user_link(_SrvId, #{data:=#{spec:=#{user:=UserId}}}=Actor) ->
    case nkactor_lib:add_link(UserId, ?GROUP_CORE, ?RES_CORE_USERS, Actor) of
        {ok, Actor2} ->
            {ok, Actor2};
        {error, Error} ->
            {error, Error}
    end;

add_user_link(_SrvId, Actor) ->
    {ok, Actor}.


%% @private
parse_post_check(List) ->
    Map1 = maps:from_list(List),
    Name = maps:get(name, Map1, <<>>),
    SurName = maps:get(surname, Map1, <<>>),
    Map2 = Map1#{
        normalized_name => nklib_parse:normalize(Name),
        normalized_surname => nklib_parse:normalize(SurName)
    },
    {ok, maps:to_list(Map2)}.


%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).