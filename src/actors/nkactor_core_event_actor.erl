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

%% @doc NkActor Event Actor
-module(nkactor_core_event_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([search/2]).
-export([config/0, parse/3]).

-include("nkactor_core.hrl").


%% ===================================================================
%% Actor behaviour
%% ===================================================================

search(SrvId, Opts) ->
    Filters1 = [
        #{field => group, value => ?GROUP_CORE},
        #{field => resource, value => ?RES_CORE_EVENTS}
    ],
    Filters2 = case Opts of
        #{start_time:=Start} ->
            [#{field=>last_update, op=>gte, value=>Start}|Filters1];
        _ ->
            Filters1
    end,
    Filters3 = case Opts of
        #{stop_time:=Stop} ->
            [#{field=>last_update, op=>lte, value=>Stop}|Filters2];
        _ ->
            Filters2
    end,
    Filters4 = case Opts of
        #{target_uid:=Target} ->
            Target2 = nkactor_lib:normalized_name(Target),
            [#{field=>name, op=>prefix, value=>Target2}|Filters3];
        _ ->
            Filters3
    end,
    Filters5 = case Opts of
        #{min_priority:=Priority} ->
            [#{field=><<"data.priority">>, op=>gte, value=>Priority, type=>integer}|Filters4];
        _ ->
            Filters4
    end,
    Spec = #{
        namespace => maps:get(namespace, Opts, <<>>),
        deep => maps:get(deep, Opts, true),
        filter => #{'and' => Filters5},
        sort => [#{field=>last_update, order=>desc}],
        from => maps:get(from, Opts, 0),
        size => maps:get(size, Opts, 100)
    },
    nkactor:search_actors(SrvId, Spec, #{}).


%% @doc
config() ->
    #{
        resource => ?RES_CORE_EVENTS,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list],
        short_names => [ev],
        activable => false,
        fields_filter => [
            'class',
            'type',
            'count',
            'priority',
            'first_timestamp',
            'last_timestamp',
            'target.uid',
            'target.group',
            'target.resource',
            'target.name',
            'target.hash'
        ],
        fields_sort => [
            'class',
            'type',
            'priority',
            'first_timestamp',
            'last_timestamp',
            'target.uid',
            'target.group',
            'target.resource',
            'target.name'
        ],
        fields_type => #{
            'count' => integer,
            'priority' => integer,
            'tags' => array
        }
    }.



%% @doc
parse(_Op, _Actor, _Req) ->
    Syntax = #{
        class => binary,
        type => binary,
        count => pos_integer,
        message => binary,
        body => map,
        priority => pos_integer,
        first_timestamp => pos_integer,
        last_timestamp => pos_integer,
        tags => {list, binary},
        target => #{
            uid => binary,
            group => binary,
            resource => binary,
            name => binary,
            hash => binary
        },
        '__mandatory' => [class, type, count, first_timestamp],
        '__defaults' => #{body => #{}, message => <<>>, tags => [], priority=>500}
    },
    {syntax, <<"v1a1">>, Syntax}.
