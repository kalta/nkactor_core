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

%% @doc Default callbacks for plugin definitions
-module(nkactor_core_api_request).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([request/3]).
-export([actor_to_api_actor/3, api_actor_to_actor/3]).

%% ===================================================================
%%
%% ===================================================================


request(_SrvId, Vsn, #{verb:=Verb, srv:=ActorSrvId}=ApiReq) ->
	try
		_SubRes = maps:get(subresource, ApiReq, []),
		Reply = nkactor_request:request(ApiReq),
		case Reply of
			{ok, Data, _ApiReq2} when Verb==get ->
				ApiActor = actor_to_api_actor(ActorSrvId, Vsn, Data),
				lager:error("NKLOG BACK ~p", [api_actor_to_actor(ActorSrvId, ApiReq, ApiActor)]),
				{ok, ApiActor}



		end
	catch
		C:E:Trace ->
			lager:error("Error in request ~p", [{C, {E, Trace}}]),
			erlang:raise(C, E, Trace)
	end.




api_actor_to_actor(SrvId, Req, ApiActor) ->
	try
		Parsed = case nklib_syntax:parse(ApiActor, api_actor_to_actor_syntax()) of
			{ok, Parsed0, _} ->
				Parsed0;
			{error, Error} ->
				throw(Error)
		end,
		ApiGroup = maps:get(group, Req, undefined),
		Group = case Parsed of
			#{<<"apiVersion">>:=ApiVsn} ->
				case binary:split(ApiVsn, <<"/">>) of
					[BodyGroup, _Vsn] when ApiGroup==BodyGroup; ApiGroup==undefined ->
						BodyGroup;
					_ ->
						throw({syntax_error, <<"apiVersion">>})
				end;
			_ ->
				ApiGroup
		end,
		ApiRes = maps:get(resource, Req, undefined),
		Res = case Parsed of
			#{<<"kind">>:=Kind} when Group /= undefined ->
				case nkactor_actor:get_module(SrvId, Group, {camel, Kind}) of
					undefined ->
						throw({syntax_error, <<"kind">>});
					Module ->
						Config = nkactor_actor:get_config(SrvId, Module),
						case maps:get(resource, Config) of
							BodyRes when BodyRes==ApiRes orelse ApiRes==undefined ->
								BodyRes;
							_ ->
								throw({syntax_error, <<"kind">>})
						end
				end;
			_ ->
				ApiRes
		end,
		Actor1 = maps:without([apivsn, kind], Parsed),
		Actor2 = case Group of
			undefined ->
				Actor1;
			_ ->
				Actor1#{group => Group}
		end,
		Actor3 = case Res of
			undefined ->
				Actor2;
			_ ->
				Actor2#{resource => Group}
		end,
		Meta = maps:get(metadata, Actor2, #{}),
		Fields1 = maps:with([uid, name, namespace], Meta),
		Actor4 = maps:merge(Actor3, Fields1),
		Meta2 = maps:without([uid, name, namespace], Meta),
		Actor4#{metadata => Meta2}
	catch
		throw:Throw ->
			{error, Throw}
	end.





api_actor_to_actor_syntax() ->
	#{
		<<"apiVersion">> => {'__key', apivsn},
		<<"kind">> => {'__key', kind},
		<<"data">> => {'__key', data},
		<<"metadata">> => {'__key', metadata, #{
			<<"uid">> => {'__key', uid},
			<<"name">> => {'__key', name},
			<<"namespace">> => {'__key', namespace},
			<<"subtype">> => {'__key', subtype},
			<<"resourceVersion">> => {'__key', hash},
			<<"generation">> => {'__key', generation},
			<<"creationTime">> => {'__key', creation_time},
			<<"updateTime">> => {'__key', update_time},
			<<"isActive">> => {'__key', is_active},
			<<"expiresTime">>=> {'__key', expires_time },
			<<"labels">> => {'__key', labels},
			<<"fts">> => {'__key', fts},
			<<"links">> => {'__key', links},
			<<"annotations">> => {'__key', annotations},
			<<"isEnabled">> => {'__key', is_enabled},
			<<"inAlaram">> => {'__key', in_alarm},
			<<"alarms">> => {'__key', alarms},
			<<"nextStatusTime">> => {'__key', next_status_time},
			<<"description">> => {'__key', description},
			<<"trace_id">> => {'__key', trace_id}
		}}
	}.






actor_to_api_actor(ActorSrvId, Vsn, Actor) ->
	#{uid:=UID, group:=Group, resource:=Res, name:=Name, namespace:=Namespace} = Actor,
	Module = nkactor_actor:get_module(ActorSrvId, Group, Res),
	Config = nkactor_actor:get_config(ActorSrvId, Module),
	#{camel:=Camel} = Config,
	{ok, ApiActor, _} = nklib_syntax:parse(Actor, actor_to_api_actor_syntax()),
	Data1 = maps:get(data, Actor, #{}),
	Data2 = [{to_bin(K), V} || {K, V}<- maps:to_list(Data1)],
	Data3 = maps:from_list(Data2),
	ApiActor2 = maps:merge(ApiActor, Data3),
	#{<<"metadata">>:=Meta1} = ApiActor,
	Meta2 = Meta1#{
		<<"uid">> => UID,
		<<"name">> => Name,
		<<"namespace">> => Namespace
	},
	ApiVsn = <<Group/binary, $/, Vsn/binary>>,
	ApiActor2#{
		<<"apiVersion">> => ApiVsn,
		<<"kind">> => Camel,
		<<"metadata">>:=Meta2
	}.




actor_to_api_actor_syntax() ->
	#{
		metadata => {'__key', <<"metadata">>, #{
			subtype => {'__key', <<"subtype">>},
			hash => {'__key', <<"resourceVersion">>},
			generation => {'__key', <<"generation">>},
			creation_time => {'__key', <<"creationTime">>},
			update_time => {'__key', <<"updateTime">>},
			is_active => {'__key', <<"isActive">>},
			expires_time => {'__key', <<"expiresTime">>},
			labels => {'__key', <<"labels">>},
			fts => {'__key', <<"fts">>},
			links => {'__key', <<"links">>},
			annotations => {'__key', <<"annotations">>},
			is_enabled => {'__key', <<"isEnabled">>},
			in_alarm => {'__key', <<"inAlaram">>},
			alarms => {'__key', <<"alarms">>},
			next_status_time => {'__key', <<"nextStatusTime">>},
			description => {'__key', <<"description">>},
			trace_id => {'__key', <<"trace_id">>}
		}}
	}.








%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).