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

%% @doc Events management module
%%
%% You can typically generate event actors from standard actor events
%% (implementing callback actor_srv_event/4) and calling make_event here
%%
%% Event will be sent asynchronously to the local event server, that will send it
%% to storage periodically


-module(nkactor_core_events).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([make_and_send_event/3, make_event/2, send_event/2, make_uid/0]).
-export([wait_for_save/1, start_link/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export([test/0]).
-export([remove_old_hashes/1]).

-include_lib("nkactor/include/nkactor.hrl").
-include_lib("nkactor_core/include/nkactor_core.hrl").
-include_lib("nkserver/include/nkserver.hrl").

-define(TIME_TO_SAVE, 1000).
-define(MAX_REMEMBER_TIME, 5*60*1000).
-define(RETRY_WAIT_TIME, 5000).
-define(MAX_RETRIES, 10).


%% ===================================================================
%% Types
%% ===================================================================

-type event() ::
	#{
		class := binary(),
		type => binary(),   % Default "normal"
		priority => integer(),
		message => binary(),
		body => map(),
		tags => [binary()],
		uid => binary()
	}.


-record(hash, {
	hash,
	counter,
	uid,
	name,
	first_time,
	created_time
}).


%% ===================================================================
%% Public - Send events
%% ===================================================================

%% @doc Generates and sends an event related to an existing actor
-spec make_and_send_event(nkserver:id(), event(), nkactor:actor()) ->
	nkactor:actor().

make_and_send_event(SrvId, Event, TargetActor) ->
	EvActor = make_event(Event, TargetActor),
	send_event(SrvId, EvActor).


%% @doc Generates an event related to an existing actor
-spec make_event(event(), nkactor:actor()) ->
	nkactor:actor().

make_event(Event, TargetActor) ->
	#{namespace:=Ns, uid:=TargetUID} = TargetActor,
	Time = nklib_date:now_bin(msecs),   % 9 bytes
	UUID = case maps:find(uid, Event) of
		{ok, UserUUID} ->
			UserUUID;
		error ->
			make_uid()
	end,
	UID = <<?RES_CORE_EVENTS/binary, $-, Time/binary, UUID/binary>>,
	Name = nkactor_lib:normalized_name(<<TargetUID/binary, $-, UUID/binary>>),
	Now = nklib_date:now_3339(usecs),
	BaseActor = #{
		uid => UID,
		group => ?GROUP_CORE,
		resource => ?RES_CORE_EVENTS,
		namespace => Ns,
		name => Name,
		data => make_event_data(Event, TargetActor),
		metadata => #{
			kind => <<"Event">>,
			creation_time => Now
		}
	},
	nkactor_lib:update(BaseActor, Now).


%% @doc
make_event_data(Event, TargetActor) ->
	#{
		name := Name,
		group := Group,
		resource := Res,
		uid := UID,
		metadata := #{hash := Hash}
	} = TargetActor,
	Time = nklib_date:now_3339(msecs),
	#{
		class => maps:get(class, Event),
		type => maps:get(type, Event, <<"normal">>),  % normal or warning
		count => 1,
		priority => maps:get(priority, Event, 500),
		message => maps:get(message, Event, <<>>),
		body => maps:get(body, Event, #{}),
		tags => maps:get(tags, Event, []),
		first_timestamp => Time,
		last_timestamp => Time,
		target => #{
			uid => UID,
			group => Group,
			resource => Res,
			name => Name,
			hash => Hash
		}
	}.


%% @doc
make_uid() ->
	<<UUID:18/binary, _/binary>> = nklib_util:luid(),
	UUID.




%% @doc Sends a new or recurring event to event server
%% If it is a recurring event, and the old versions have not been sent yet,
%% only one will be sent with updated counters
-spec send_event(nkserver:id(), nkactor:actor()) ->
	nkactor:actor().

send_event(SrvId, EvActor) ->
	{_Op, EvActor2, Hash} = update_counter(EvActor),
	gen_server:cast(get_pid(SrvId), {new_event, Hash, EvActor2}),
	EvActor.


%% @private
wait_for_save(SrvId) ->
	gen_server:call(get_pid(SrvId), wait_for_save, infinity).


%% @private
get_pid(SrvId) ->
	[{_, Pid}] = nklib_proc:values({?MODULE, SrvId}),
	Pid.


% ===================================================================
%% gen_server behaviour
%% ===================================================================


-record(state, {
	srv :: nkserver:id(),
	to_save = #{} :: #{Hash::integer() => #actor_id{}},
	wait_save = []
}).


%% @doc
-spec start_link(nkserver:id()) ->
	{ok, pid()} | {error, term()}.

start_link(SrvId) ->
	gen_server:start_link(?MODULE, [SrvId], []).


%% @private
-spec init(term()) ->
	{ok, tuple()} | {ok, tuple(), timeout()|hibernate} |
	{stop, term()} | ignore.

init([SrvId]) ->
	self() ! do_save,
	true = nklib_proc:reg({?MODULE, SrvId}),
	% ets table is created in supervisor
	{ok, #state{srv=SrvId}}.


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
	{noreply, #state{}} | {reply, term(), #state{}} |
	{stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(wait_for_save, From, #state{wait_save=Wait}=State) ->
	{noreply, State#state{wait_save=[From|Wait]}};

handle_call(Msg, _From, State) ->
	lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
	{noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
	{noreply, #state{}} | {stop, term(), #state{}}.

handle_cast({new_event, Hash, Event}, #state{to_save=ToSave}=State) ->
	ToSave2 = ToSave#{Hash => Event},
	{noreply, State#state{to_save=ToSave2}};

handle_cast(Msg, State) ->
	lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
	{noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
	{noreply, #state{}} | {stop, term(), #state{}}.

handle_info(do_save, #state{srv=SrvId, to_save=ToSave, wait_save=Wait}=State) ->
	remove_old_hashes(?MAX_REMEMBER_TIME div 1000),
	case maps:values(ToSave) of
		[] ->
			ok;
		Actors ->
			do_save(SrvId, Actors, ?MAX_RETRIES)
	end,
	lists:foreach(
		fun(From) -> gen_server:reply(From, ok) end,
		lists:reverse(Wait)),
	erlang:send_after(?TIME_TO_SAVE, self(), do_save),
	{noreply, State#state{wait_save=[], to_save = #{}}};

handle_info(Info, State) ->
	lager:warning("Module ~p received unexpected info: ~p (~p)", [?MODULE, Info, State]),
	{noreply, State}.


%% @private
-spec code_change(term(), #state{}, term()) ->
	{ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
	ok.

terminate(_Reason, _State) ->
	ok.



% ===================================================================
%% Internal
%% ===================================================================


%% @private Generates a hash from the actor, and stores it in local ets if not there
%% If it is not first iteration, increments the counter and modifies actor
update_counter(#{data:=Data, metadata:=Meta}=Actor) ->
	HashInfo = get_hash(Actor),
	#hash{hash=Hash} = HashInfo,
	Counter2 = ets:update_counter(?MODULE, Hash, {#hash.counter, 1}, HashInfo),
	case Counter2 of
		1 ->
			{created, Actor, Hash};
		_ ->
			[HashInfo2] = ets:lookup(?MODULE, Hash),
			#hash{uid=UID, name=Name, first_time=First, created_time=Create} = HashInfo2,
			Data2 = Data#{count:=Counter2, first_timestamp:=First},
			Meta2 = Meta#{creation_time:=Create, generation:=Counter2-1},
			Actor2 = Actor#{uid:=UID, name:=Name, data:=Data2, metadata:=Meta2},
			{updated, Actor2, Hash}
	end.


%% @private
get_hash(#{uid:=UID, name:=Name, data:=Data, metadata:=Meta}=Actor) ->
	Data2 = Data#{
		first_timestamp := <<>>,
		last_timestamp := <<>>
	},
	0 = maps:get(generation, Meta),
	Meta2 = Meta#{
		creation_time := <<>>,
		update_time := <<>>
	},
	Actor2 = Actor#{
		uid := <<>>,
		name := <<>>,
		data => Data2,
		metadata => Meta2,
		hash => <<>>
	},
	#hash{
		hash = erlang:phash2(Actor2),
		counter = 0,
		uid = UID,
		name = Name,
		first_time = maps:get(first_timestamp, Data),
		created_time = maps:get(creation_time, Meta)
	}.


%% @private
%% Remove hashes with firstTime older that Secs
remove_old_hashes(Secs) ->
	Max = nklib_date:epoch(usecs) - Secs*1000000,
	{ok, Time} = nklib_date:to_3339(Max, usecs),
	{Time, ets:select_delete(?MODULE, [{{hash, '$1','$2','$3','$4','$5','$6'},[],[{'<','$5',Time}]}])}.


%% @private
do_save(SrvId, Actors, Tries) when Tries > 0 ->
	?CALL_SRV(SrvId, actor_core_events_saved, [SrvId, Actors]),
	ok;

%%%% @private
%%do_save(SrvId, Actors, Tries) when Tries > 0 ->
%%	nkserver_trace:trace("calling events actor_db_update"),
%%	case ?CALL_SRV(SrvId, actor_db_create, [SrvId, Actors, #{}]) of
%%		{ok, _Meta} ->
%%			?CALL_SRV(SrvId, actor_core_events_saved, [SrvId, Actors]),
%%			nkserver_trace:trace("events saved"),
%%			ok;
%%		{error, Error} ->
%%			nkserver_trace:log(warning, "could not save events: ~p (~p tries left)", [Error, Tries]),
%%			timer:sleep(?RETRY_WAIT_TIME),
%%			do_save(SrvId, Actors, Tries-1)
%%	end;

do_save(_SrvId, _Actors, _Tries) ->
	error.


%% ===================================================================
%% EUnit tests
%% ===================================================================

test() ->
	remove_old_hashes(0),
	Base = nklib_date:epoch(secs) - 6,
	{ok, Time1} = nklib_date:to_3339(Base*1000, msecs),
	Data1 = #{
		type => <<"type1">>,
		class => <<"class1">>,
		count => 1,
		message => <<"message1">>,
		body => #{<<"a">> => 1},
		tags => [<<"ab">>],
		first_timestamp => Time1,
		last_timestamp => Time1,
		target => #{
			uid => <<"target1">>
		}
	},
	Meta1 = #{
		creation_time => Time1,
		update_time => Time1,
		generation => 0
	},
	EventActor1 = #{
		uid => <<"uid1">>,
		name => <<"name1">>,
		data => Data1,
		metadata => Meta1
	},
	% This is the first message in the series
	{created, EventActor1, Hash1} = update_counter(EventActor1),

	% Second message, a second later
	{ok, Time2} = nklib_date:to_3339(Base*1000+1, msecs),
	Data2 = Data1#{first_timestamp => Time2, last_timestamp => Time2},
	Meta2 = Meta1#{creation_time => Time2, update_time => Time2},
	EventActor2 = EventActor1#{
		uid := <<"uid2">>,
		name := <<"name2">>,
		data := Data2,
		metadata := Meta2
	},
	{updated, EventActor3, Hash1} = update_counter(EventActor2),
	Data3 = Data1#{count:=2, first_timestamp:=Time1, last_timestamp:=Time2},
	Meta3 = Meta2#{creation_time:=Time1, generation:=1},
	#{
		uid := <<"uid1">>,
		name := <<"name1">>,
		data := Data3,
		metadata := Meta3
	} = EventActor3,

	% Third, in the same second
	EventActor4 = EventActor2#{uid := <<"uid4">>, name := <<"name4">>},
	{updated, EventActor5, Hash1} = update_counter(EventActor4),
	Data5 = Data3#{count:=3},
	Meta5 = Meta3#{generation:=2},
	#{
		uid := <<"uid1">>,
		name := <<"name1">>,
		data := Data5,
		metadata := Meta5
	} = EventActor5,

	% If we change something, it is another message
	EventActor6 = EventActor4#{
		uid := <<"uid6">>,
		name := <<"name6">>,
		data := Data1,
		metadata := Meta1#{<<"field1">> => <<"value2">>}
	},
	{created, EventActor6, Hash2} = update_counter(EventActor6),
	true = Hash2 /= Hash1,


	% We send the previous again, a second later
	{ok, Time7} = nklib_date:to_3339(Base*1000+2, msecs),
	Data7 = Data1#{first_timestamp:=Time7, last_timestamp:=Time7},
	Meta7 = Meta1#{
		creation_time => Time7,
		update_time => Time7
	},
	EventActor7 = #{
		uid => <<"uid7">>,
		name => <<"name7">>,
		data => Data7,
		metadata => Meta7,
		hash => <<"3">>
	},
	{ProcT1, {updated, Actor8, Hash1}} = timer:tc(fun() -> update_counter(EventActor7) end),
	io:format("Update1 time: ~pusecs\n", [ProcT1]),
	Data8 = Data1#{count:=4, first_timestamp:=Time1, last_timestamp:=Time7},
	Meta8 = Meta7#{creation_time:=Time1, generation:=3},
	#{
		uid := <<"uid1">>,
		name := <<"name1">>,
		data := Data8,
		metadata := Meta8
	} = Actor8,


	% We reset timers older than 1 sec, counter is 1 again
	{_, 2} = remove_old_hashes(0),
	{ProcT2, {created, EventActor7, Hash1}} = timer:tc(fun() -> update_counter(EventActor7) end),
	{_, 1} = remove_old_hashes(0),
	io:format("Update2 time: ~pusecs\n", [ProcT2]),
	ok.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
	test().

-endif.

