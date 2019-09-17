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

%% @doc NkActor Task Actor
-module(nkactor_core_task_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([config/0, parse/3, request/4, init/2, event/2,
         sync_op/3, async_op/2, stop/2]).
-export_type([event/0]).

-include_lib("nkactor/include/nkactor.hrl").
-include_lib("nkactor/include/nkactor_debug.hrl").
-include("nkactor_core.hrl").

-define(DEFAULT_MAX_TRIES, 3).
-define(DEFAULT_MAX_TASK_SECS, 60*60).

%% ===================================================================
%% Types
%% ===================================================================


-type event() ::
    {updated_state, map()}.


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_TASKS,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list, update, watch],
        auto_activate => true,
        fields_filter => [
            'status.last_try_start_time',
            'status.tries'
        ],
        fields_sort => [
            'status.last_try_start_time',
            'status.tries'
        ],
        fields_type => #{
            'status.tries' => integer
        }
    }.


%% @doc
parse(Op, Actor, _Req) ->
    Syntax = #{
        spec => #{
            job => map,
            max_tries => pos_integer,
            max_secs => pos_integer,
            '__defaults' => #{
                max_tries => ?DEFAULT_MAX_TRIES,
                max_secs => ?DEFAULT_MAX_TASK_SECS
            }
        },
        status => #{
            task_status => {atom, [init, start, progress, error, success, failure]},
            tries => pos_integer,
            last_try_start_time => binary,
            last_status_time => binary,
            progress => {integer, 0, 100},
            error_msg => binary
        },
        '__mandatory' => [spec]
    },
    % Set expires_time based on max_secs
    case nkactor_lib:parse_actor_data(Op, Actor, <<"v1a1">>, Syntax) of
        {ok, #{data:=#{spec:=Spec}}=Actor2} ->
            #{max_secs:=MaxSecs} = Spec,
            % If no expires yet, we set it
            Actor3 = nkactor_lib:maybe_set_ttl(Actor2, 1000*MaxSecs),
            {ok, Actor3};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
request(update, <<"_state">>, ActorId, Req) ->
    Body = maps:get(body, Req, #{}),
    case nkactor:sync_op(ActorId, {update_state, Body}) of
        ok ->
            {status, actor_updated};
        {error, Error} ->
            {error, Error}
    end;

request(_Verb, _Path, _ActorId, _Req) ->
    continue.


%% @doc
%% - generate initial run_state
%% - check if the numbers of tries have been exceeded,
%%   and delete the actor if exceeded
%% - program 'updated_state' event with progress=start (see event/2)
%% - save updated status
init(_Op, #actor_st{unload_policy={expires, _}, actor=Actor}=ActorSt) ->
    #{data:=Data} = Actor,
    Status = maps:get(status, Data, #{}),
    Tries = maps:get(tries, Status, 0),
    #{spec := #{max_tries:=MaxTries}} = Data,
    case Tries >= MaxTries of
        false ->
            Now = nklib_date:now_3339(msecs),
            Status2 = Status#{
                task_status => init,
                last_try_start_time => Now,
                tries => Tries+1,
                progress => 0
            },
            Actor2 = Actor#{data:=Data#{status => Status2}},
            % We save the new status to disk
            ActorSt2 = ActorSt#actor_st{actor=Actor2, is_dirty=true},
            % We don't want to call set_run_state/2 yet, because the start
            % event would arrive before the creation event
            nkactor:async_op(self(), {update_state, #{task_status=>start}}),
            {ok, ActorSt2};
        true ->
            Status2 = Status#{
                task_status =>  failure,
                error_msg => <<"task_max_tries_reached">>
            },
            % Allow in-queue events to be processed
            % timer:sleep(100),
            ActorSt2 = set_status(Status2, ActorSt),
            ?ACTOR_LOG(notice, "max tries reached for task", [], ActorSt2),
            {delete, task_max_tries_reached}
    end;

init(_Op, _ActorSt) ->
    {error, expires_missing}.



%%%% @doc Called on every event launched at this actor (our's or not)
%%%% Used to generate API events
%%event({updated_state, UpdStatus}, #actor_st{actor=Actor}=ActorSt) ->
%%    #actor{data=Data} = Actor,
%%    #{
%%        spec := #{max_tries := MaxTries},
%%        <<"status">> := #{<<"tries">> := Tries}
%%    } = Data,
%%    #{status:=Status} = UpdStatus,
%%    ApiEvBody = #{
%%        <<"tries">> => Tries,
%%       max_tries => MaxTries
%%    },
%%    ActorSt2 = case Status of
%%        init ->
%%            ActorSt;
%%        start ->
%%            % For 'start', include all spec in API event body
%%            Spec = maps:get(spec, Data, #{}),
%%            ApiEvBody2 = maps:merge(Spec, ApiEvBody),
%%            ApiEv = #{reason => <<"TaskStart">>, body => ApiEvBody2},
%%            nkactor_core_actor_util:api_event(ApiEv, ActorSt);
%%        progress ->
%%            ActorSt;
%%        error ->
%%            ErrMsg = maps:get(errorMsg, UpdStatus, <<>>),
%%            ApiEv = #{reason => <<"TaskError">>, message=>ErrMsg, body => ApiEvBody},
%%            nkactor_core_actor_util:api_event(ApiEv, ActorSt);
%%        success ->
%%            ApiEv = #{reason => <<"TaskSuccess">>, body => ApiEvBody},
%%            nkactor_core_actor_util:api_event(ApiEv, ActorSt);
%%        failure ->
%%            ErrMsg = maps:get(errorMsg, UpdStatus, <<>>),
%%            ApiEv = #{reason => <<"TaskFaillure">>, message=>ErrMsg, body => ApiEvBody},
%%            nkactor_core_actor_util:api_event(ApiEv, ActorSt)
%%    end,
%%    {ok, ActorSt2};

event(_Event, _ActorSt) ->
    continue.


%% @doc
sync_op({update_state, Body}, _From, ActorSt) ->
    {Reply, ActorSt2} = do_update_state(Body, ActorSt),
    {reply_and_save, Reply, ActorSt2};

sync_op(_Op, _From, _ActorSt) ->
    continue.


%% @doc
async_op({update_state, Body}, ActorSt) ->
    {_Reply, ActorSt2} = do_update_state(Body, ActorSt),
    {noreply_and_save, ActorSt2};

async_op(_Op, _ActorSt) ->
    continue.


%% @doc
stop(actor_expired, ActorSt) ->
    Status = #{
        status => failure,
        error_msg => <<"task_max_time_reached">>
    },
    ActorSt2 = set_status(Status, ActorSt),
    ?ACTOR_LOG(notice, "max time reached for task", [], ActorSt2),
    {delete, ActorSt2};

stop(_Reason, ActorSt) ->
    {ok, ActorSt}.



%% ===================================================================
%% Internal
%% ===================================================================

%% @doc
do_update_state(Body, ActorSt) ->
    Syntax = #{
        task_status => {atom, [start, progress, error, success, failure]},
        progress => {integer, 0, 100},
        error_msg => binary,
        '__mandatory' => [task_status]
    },
    case nkactor_lib:parse(Body, Syntax, false) of
        {ok, Status} ->
            {ok, set_status(Status, ActorSt)};
        {error, Error} ->
            {{error, Error}, ActorSt}
    end.


%% @private
%% - Sets a new run_state
%% - Send event
set_status(Status, #actor_st{actor=#{data:=Data}=Actor}=ActorSt) ->
    OldStatus = maps:get(status, Data, #{}),
    NewStatus1 = maps:merge(OldStatus, Status),
    Now = nklib_date:now_3339(secs),
    NewStatus2 = NewStatus1#{last_status_time => Now},
    % On expired, it may have no status
    IsActive = case maps:get(task_status, NewStatus2, start) of
        success ->
            % Allow events
            nkactor_srv:delayed_async_op(self(), delete, 100),
            false;
        error ->
            nkactor_srv:delayed_async_op(self(), {stop, task_status_error}, 100),
            false;
        failure ->
            nkactor_srv:delayed_async_op(self(), delete, 100),
            false;
        _ ->
            true
    end,
    ActorSt2 = ActorSt#actor_st{actor=Actor#{data:=Data#{status => NewStatus2}}},
    ActorSt3 = nkactor_srv_lib:set_active(IsActive, ActorSt2),
    Event = {updated_state, NewStatus2},
    % Sleep so that it won't go to the same msec
    timer:sleep(2),
    nkactor_srv_lib:event(Event, ActorSt3).








