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

%% @doc NkActor Cache
%%
%% Typically, we capture created and deleted events is callbacks to
%% update target reference to us

-module(nkactor_core_cache_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([find_by_target/2, get_infos/1, get_info/1]).
-export([op_get_info/1, op_update/2]).
-export([config/0, parse/3, init/2, get/2, sync_op/3, async_op/2, handle_info/2]).
-import(nkserver_trace, [trace/1, log/3]).

-include_lib("nkserver/include/nkserver.hrl").
-include_lib("nkactor/include/nkactor.hrl").
-include("nkactor_core.hrl").

-define(DEFAULT_TTL, 60*60*1000).

-type info() ::
    #{
        uid => nkactor:uid(),
        class => binary(),
        type := binary(),
        meta => map()
    }.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc
find_by_target(SrvId, TargetUID) ->
    Opts = #{
        namespace => <<>>,
        deep => true,
        link_type => ?LINK_TARGET_CACHE,
        size => 1000
    },
    case nkactor:search_linked_to(SrvId, TargetUID, Opts) of
        {ok, List} ->
            {ok, [UID || {UID, _} <- List]};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec get_infos([nkactor:uid()]) -> [info()].
get_infos(UIDs) ->
    get_infos(lists:sort(UIDs), []).


%% @private
get_infos([], Acc) ->
    lists:reverse(Acc);

get_infos([UID|Rest], Acc) ->
    case op_get_info(UID) of
        {ok, Info} ->
            get_infos(Rest, [Info|Acc]);
        {error, Error} ->
            log(warning, "could not read cronjob ~s: ~p", [UID, Error]),
            get_infos(Rest, Acc)
    end.


%% @doc
get_info(#{uid:=UID, data:=#{spec:=Spec}}) ->
    Info = maps:with([class, type, meta], Spec),
    Info#{uid => UID}.


%% @doc
op_update(Id, Update) ->
    nkactor:async_op(Id, {nkactor_update_cache, Update}).


%% @doc
op_get_info(Id) ->
    nkactor:sync_op(Id, nkactor_get_info).


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_CACHE,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list, update],
        permanent => true
    }.


%% @doc
parse(_Op, _Actor, _Req) ->
    Spec = #{
        class => binary,
        type => binary,
        target_group => binary,
        target_resource => binary,
        target_uid => binary,
        ttl => pos_integer,
        meta => map,
        '__mandatory' => [class],
        '__defaults' => #{meta => #{}}
    },
    {syntax, <<"v1a1">>, #{spec => Spec}}.


%% @private
init(_, ActorSt) ->
    self() ! nkactor_load,
    ActorSt2 = ActorSt#actor_st{run_state=#{ttl_timer=>undefined, data=>#{}, loaded_time=>0}},
    case do_link_target(ActorSt2) of
        {ok, ActorSt3} ->
            {ok, set_ttl(ActorSt3)};
        {error, Error} ->
            {error, Error}
    end.


get(Actor, #actor_st{run_state=RunState}=ActorSt) ->
    Extra = maps:with([data, loaded_time], RunState),
    {ok, Actor#{cache=>Extra}, set_ttl(ActorSt)}.


%% @doc
sync_op(nkactor_get_info, _From, #actor_st{actor=Actor}=ActorSt) ->
    {reply, {ok, get_info(Actor)}, ActorSt};

sync_op(_Op, _From, _ActorSt) ->
    continue.


%% @private
async_op({nkactor_update_cache, Update}, ActorSt) ->
    #actor_st{srv=SrvId, actor=Actor, run_state=#{data:=Data}=RunState} = ActorSt,
    #{data:=#{spec:=#{class:=Class}}} = Actor,
    case ?CALL_SRV(SrvId, actor_core_cache_update, [SrvId, Class, Update, Data, Actor]) of
        {ok, Data2} ->
            ActorSt2 = ActorSt#actor_st{run_state=RunState#{data:=Data2}},
            {noreply, set_ttl(ActorSt2)};
        {error, Error} ->
            {stop, Error, ActorSt}
    end;

async_op(_, _) ->
    continue.


%% @private
handle_info(nkactor_load, #actor_st{srv=SrvId, run_state=RunState, actor=Actor}=ActorSt) ->
    #{data:=#{spec:=#{class:=Class}}} = Actor,
    case ?CALL_SRV(SrvId, actor_core_cache_load, [SrvId, Class, Actor]) of
        {ok, Data2} ->
            Now = nklib_date:epoch(msecs),
            {noreply, ActorSt#actor_st{run_state=RunState#{data:=Data2, loaded_time:=Now}}};
        {error, Error} ->
            {stop, Error, ActorSt}
    end;

handle_info(nkactor_expired, ActorSt) ->
    {stop, normal, ActorSt};

handle_info(_, _) ->
    continue.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_link_target(#actor_st{actor=#{data:=#{spec:=Spec}}=Actor}=ActorSt) ->
    case Spec of
        #{target_group:=Group, target_resource:=Res, target_uid:=UID} ->
            case nkactor_lib:add_checked_link(UID, Group, Res, Actor, ?LINK_TARGET_CACHE) of
                {ok, _, Actor2} ->
                    {ok, ActorSt#actor_st{actor=Actor2}};
                {error, Error} ->
                    {error, Error}
            end;
        #{target_uid:=UID} ->
            case nkactor_lib:add_checked_link(UID, Actor, ?LINK_TARGET_CACHE) of
                {ok, #actor_id{group=Group, resource=Res}, Actor2} ->
                    #{data:=#{spec:=Spec2}=Data2} = Actor2,
                    Spec3 = Spec2#{target_group=>Group, target_resource=>Res},
                    Actor3 = Actor2#{data:=Data2#{spec:=Spec3}},
                    {ok, ActorSt#actor_st{actor=Actor3}};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {ok, ActorSt}
    end.


set_ttl(ActorSt) ->
    #actor_st{actor=#{data:=#{spec:=Spec}}, run_state=#{ttl_timer:=Timer}=RunState} = ActorSt,
    nklib_util:cancel_timer(Timer),
    TTL = maps:get(ttl, Spec, ?DEFAULT_TTL),
    Timer2 = erlang:send_after(TTL, self(), nkactor_expired),
    ActorSt#actor_st{run_state=RunState#{ttl_timer:=Timer2}}.


