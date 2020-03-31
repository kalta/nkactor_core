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

%% @doc NkActor CoreJob Actor
%% Can block during call to actor_core_cronjobs_activate, spawn there if possible
-module(nkactor_core_cronjob_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([find_target_cronjobs/2, get_info/1]).
-export([op_get_info/1, op_update_meta/2, op_update_schedule/2]).
-export([config/0, parse/3, activated/2, init/2, update/3, sync_op/3]).
-export_type([info/0, event/0]).
-import(nkactor_srv_lib, [new_actor_span/4, event/3]).
-import(nkserver_trace, [trace/1, log/3]).

-include_lib("nkactor/include/nkactor.hrl").
-include_lib("nkactor/include/nkactor_debug.hrl").
-include_lib("nkserver/include/nkserver.hrl").
-include("nkactor_core.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type info() ::
    #{
        uid := nkactor:uid(),
        class => binary(),
        type => binary(),
        schedule := nklib_schedule:params(),
        next_fire_time := null | binary(),
        last_fire_time := null | binary(),
        expired := boolean,
        meta := map
    }.

-type event() ::
    fire_time_updated.




%% ===================================================================
%% External
%% ===================================================================


%% @doc
find_target_cronjobs(SrvId, TargetUID) ->
    Opts = #{
        namespace => <<>>,
        deep => true,
        link_type => ?LINK_TARGET_CRONJOB,
        size => 1000
    },
    case nkactor:search_linked_to(SrvId, TargetUID, Opts) of
        {ok, List} ->
            {ok, do_find_cronjobs(List, [])};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_find_cronjobs([], Acc) ->
    Acc;

do_find_cronjobs([{UID, _}|Rest], Acc) ->
    case op_get_info(UID) of
        {ok, Info} ->
            do_find_cronjobs(Rest, [Info|Acc]);
        {error, actor_not_found} ->
            do_find_cronjobs(Rest, Acc);
        {error, Error} ->
            lager:warning("Could not read cronjob ~s: ~p", [UID, Error]),
            do_find_cronjobs(Rest, Acc)
    end.


%% @doc
get_info(#{uid:=UID, data:=#{spec:=Spec, status:=Status}}) ->
    Spec2 = maps:with([class, type, schedule, meta], Spec),
    Status2 = maps:with([next_fire_time, last_fire_time, expired], Status),
    Info = maps:merge(Spec2, Status2),
    Info#{uid => UID}.


%% @doc
op_update_meta(Id, Meta) when is_map(Meta) ->
    nkactor:sync_op(Id, {nkactor_update_meta, Meta}).


%% @doc
op_update_schedule(Id, Meta) when is_map(Meta) ->
    nkactor:sync_op(Id, {nkactor_update_schedule, Meta}).


%% @doc
op_get_info(Id) ->
    nkactor:sync_op(Id, nkactor_get_info).




%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_CRON_JOBS,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list, update],
        camel => <<"CronJob">>,
        fields_filter => [
            'spec.class',
            'spec.type',
            'spec.target_group',
            'spec.target_resource',
            'spec.target_uid',
            'status.next_fire_time',
            'status.last_fire_time',
            'status.expired'
        ],
        fields_sort => [
            'spec.class',
            'spec.type',
            'spec.target_group',
            'spec.target_resource',
            'spec.target_uid',
            'status.next_fire_time',
            'status.last_fire_time',
            'status.expired'
        ],
        fields_type => #{
            'status.next_fire_time' => string_null,
            'status.last_fire_time' => string_null,
            'status.expired' => boolean
        },
        fields_static => [
            'spec.class',
            'spec.type',
            'spec.target_group',
            'spec.target_resource',
            'spec.target_uid'
        ]
    }.


%% @doc
parse(_Op, _Actor, _Req) ->
    Syntax = #{
        spec => #{
            class => binary,
            type => binary,
            target_group => binary,
            target_resource => binary,
            target_uid => binary,
            schedule => fun nklib_schedule:parse/1,
            meta => map,
            '__mandatory' => [schedule],
            '__defaults' => #{
                meta => #{}
            }
        },
        status => #{
            next_fire_time => [{atom, [null]}, binary],
            last_fire_time => [{atom, [null]}, binary],
            expired => boolean
        },
        '__mandatory' => [spec]
    },
    {syntax, <<"v1a1">>, Syntax}.


%% @doc
activated(_Time, ActorSt) ->
    ActorSt2 = do_activate(ActorSt),
    {ok, ActorSt2}.


%% @doc
init(create, #actor_st{actor=#{data:=Data}=Actor}=ActorSt) ->
    Status1 = maps:get(status, Data, #{}),
    Base = #{
        next_fire_time => null,
        last_fire_time => null,
        expired => false
    },
    Status2 = maps:merge(Base, Status1),
    Actor2 = Actor#{data:=Data#{status=>Status2}},
    case do_link_target(ActorSt#actor_st{actor=Actor2}) of
        {ok, ActorSt2} ->
            {ok, do_update(ActorSt2)};
        {error, Error} ->
            {error, Error}
    end;

init(start, ActorSt) ->
    {ok, do_update(ActorSt)}.


%% @doc
update(Actor, _Opts, ActorSt) ->
    Actor2 = nkactor_srv_lib:update_status(Actor, ActorSt),
    {ok, do_update(ActorSt#actor_st{actor=Actor2})}.


%% @doc
sync_op(nkactor_get_info, _From, #actor_st{actor=Actor}=ActorSt) ->
    {reply, {ok, get_info(Actor)}, ActorSt};

sync_op({nkactor_update_meta, Meta}, _From, #actor_st{actor=Actor}=ActorSt) ->
    #{data:=#{spec:=Spec}=Data} = Actor,
    case Spec of
        #{meta:=Meta} ->
            {reply, ok, ActorSt};
        _ ->
            Spec2 = Spec#{meta:=Meta},
            Actor2 = Actor#{data:=Data#{spec:=Spec2}},
            case nkactor_srv_lib:update(Actor2, #{}, ActorSt) of
                {ok, ActorSt2} ->
                    {reply, ok, ActorSt2};
                {error, Error, ActorSt2} ->
                    {reply, {error, Error}, ActorSt2}
            end
    end;

sync_op({nkactor_update_schedule, Schedule}, _From, #actor_st{actor=Actor}=ActorSt) ->
    case nklib_schedule:parse(Schedule) of
        {ok, Parsed} ->
            #{data:=#{spec:=Spec}=Data} = Actor,
            case Spec of
                #{schedule:=Parsed} ->
                    {reply, ok, ActorSt};
                _ ->
                    Spec2 = Spec#{schedule:=Parsed},
                    Actor2 = Actor#{data:=Data#{spec:=Spec2}},
                    case nkactor_srv_lib:update(Actor2, #{}, ActorSt) of
                        {ok, ActorSt2} ->
                            {reply, ok, ActorSt2};
                        {error, Error, ActorSt2} ->
                            {reply, {error, Error}, ActorSt2}
                    end
            end;
        {error, Error} ->
            {reply, {error, Error}, ActorSt}
    end;

sync_op(_Op, _From, _ActorSt) ->
    continue.




%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_link_target(#actor_st{actor=#{data:=#{spec:=Spec}}=Actor}=ActorSt) ->
    case Spec of
        #{target_group:=Group, target_resource:=Res, target_uid:=UID} ->
            case nkactor_lib:add_checked_link(UID, Group, Res, Actor, ?LINK_TARGET_CRONJOB) of
                {ok, _, Actor2} ->
                    {ok, ActorSt#actor_st{actor=Actor2}};
                {error, Error} ->
                    {error, Error}
            end;
        #{target_uid:=UID} ->
            case nkactor_lib:add_checked_link(UID, Actor, ?LINK_TARGET_CRONJOB) of
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


%% @private
do_update(#actor_st{actor=Actor}=ActorSt) ->
    #{data:=#{spec:=#{schedule:=Schedule}}=Data} = Actor,
    Status = maps:get(status, Data, #{}),
    FireTime = maps:get(next_fire_time, Status),
    Expired = maps:get(expired, Status),
    Now = nklib_date:now_3339(secs),
    case nklib_schedule:next_fire_time2(Now, Schedule, Status) of
        <<>> when FireTime == null, Expired == true ->
            ActorSt;
        <<>> ->
            Status2 = Status#{
                next_fire_time => null,
                expired => true
            },
            Actor2 = Actor#{data:=Data#{status:=Status2}},
            ActorSt2 = ActorSt#actor_st{actor=Actor2, is_dirty=true},
            ActorSt3 = nkactor_srv_lib:unset_activate_time(ActorSt2),
            ActorSt4 = event(fire_time_updated, #{fire_time=>null}, ActorSt3),
            ActorSt4;
        FireTime ->
            ActorSt;
        NextFireTime ->
            Status2 = Status#{
                next_fire_time => NextFireTime,
                expired => false
            },
            Actor2 = Actor#{data:=Data#{status:=Status2}},
            ActorSt2 = ActorSt#actor_st{actor=Actor2, is_dirty=true},
            ActorSt3 = nkactor_srv_lib:set_activate_time(NextFireTime, ActorSt2),
            ActorSt4 = event(fire_time_updated, #{fire_time=>NextFireTime}, ActorSt3),
            ActorSt4
    end.


%% @private
do_activate(#actor_st{srv=SrvId, actor=Actor}=ActorSt) ->
    #{data:=#{spec:=Spec, status:=#{next_fire_time:=FireTime}}} = Actor,
    Now = nklib_date:now_3339(usecs),
    case Now > FireTime of
        true ->
            Fun = fun() ->
                trace("calling actor_cronjob_activate"),
                Args = [
                    maps:get(class, Spec, undefined),
                    maps:get(type, Spec, undefined),
                    maps:get(target_uid, Spec, undefined),
                    Actor
                ],
                Actor2 = try
                    {ok, ActorAct} = ?CALL_SRV(SrvId, actor_core_cronjobs_activate, Args),
                    #{data:=#{status:=Status}=Data} = ActorAct,
                    log(debug, "new last_fire_time: ~s", [Now]),
                    Status2 = Status#{last_fire_time => Now},
                    ActorAct#{data:=Data#{status:=Status2}}
                catch
                    Class:Reason:Stack ->
                        lager:warning("Exception calling actor_core_cronjobs_activate: ~p ~p (~p)", [Class, Reason, Stack]),
                    Actor
                end,
                do_update(ActorSt#actor_st{actor=Actor2})
            end,
            new_actor_span(cronjob_fired, Fun, #{}, ActorSt);
        false ->
            log(warning, "called activate for previous date!", []),
            ActorSt
    end.





