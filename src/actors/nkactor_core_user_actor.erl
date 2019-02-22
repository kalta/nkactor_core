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

%% @doc NkActor User Actor
-module(nkactor_core_user_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([op_check_pass/2]).
-export([config/0, parse/2, sync_op/3, get/2, request/4]).
-export([store_pass/1]).

-include_lib("nkactor/include/nkactor.hrl").
-include("nkactor_core.hrl").
-include_lib("nkpacket/include/nkpacket.hrl").

-define(MAGIC_PASS, <<226,141,134,226,132,153,226,141,133>>). %%  "⍆ℙ⍅"/utf8
-define(INVALID_PASS_SLEEP, 250).



%% ===================================================================
%% External
%% ===================================================================

op_check_pass(UserId, Pass) ->
    case nkactor:sync_op(UserId, {nkactor_check_pass, Pass}) of
        {ok, Bool} ->
            {ok, Bool};
        {error, Error} ->
            {error, Error}
    end.



%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_USERS,
        versions => [<<"0">>],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch],
        short_names => [u]
    }.


%% @doc
parse(_Actor, _Req) ->
    Fun = fun(Pass) ->
        StoredPass = store_pass(Pass),
        {ok, StoredPass}
    end,
    {syntax, #{spec=>#{password => Fun}}}.


%% @doc
request(get, [<<"_rpc">>, <<"checkpass">>], ActorId, Req) ->
    Syntax = #{password => binary, '__mandatory'=>password},
    case nkactor_lib:parse_request_params(Req, Syntax) of
        {ok, #{password:=Pass}} ->
            case op_check_pass(ActorId, Pass) of
                {ok, true} ->
                    {status, password_valid};
                {ok, false} ->
                    timer:sleep(?INVALID_PASS_SLEEP),
                    {status, password_invalid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

request(_Verb, _Path, _ActorId, _Req) ->
    continue.


%% @doc 
get(Actor, ActorSt) ->
    #{data:=Data} = Actor,
    Actor2 = case Data of
        #{spec:=#{password:=_}=Spec} ->
            Actor#{data:=Data#{spec:=Spec#{password => <<>>}}};
        _ ->
            Actor
    end,
    {ok, Actor2, ActorSt}.


%% @doc
sync_op({nkactor_check_pass, Pass}, _From, ActorSt) ->
    #actor_st{actor=#{data:=Data}} = ActorSt,
    Valid = case Data of
        #{spec:=#{password:=Stored}} ->
            store_pass(Pass) == Stored;
        _ ->
            false
    end,
    Result =  {ok, Valid},
    {reply, Result, ActorSt};

sync_op(_Op, _From, _ActorSt) ->
    continue.


%% ===================================================================
%% Internal
%% ===================================================================


%% @doc Generates a password from an user password or hash
-spec store_pass(string()|binary()) ->
    binary().

store_pass(Pass) ->
    Pass2 = to_bin(Pass),
    case binary:split(Pass2, ?MAGIC_PASS) of
        [<<>>, _] ->
            Pass2;
        _ ->
            Salt = <<"netcomposer">>,
            Iters = nkactor_core_app:get(pbkdf_iters),
            {ok, Pbkdf2} = pbkdf2:pbkdf2(sha, Pass2, Salt, Iters),
            Hash = nklib_util:lhash(Pbkdf2),
            <<?MAGIC_PASS/binary, Hash/binary>>
    end.


%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).