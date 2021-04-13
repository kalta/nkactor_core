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

%% @doc NkActor User Actor
-module(nkactor_core_user_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([find_login/3, write_pass/2, has_role/3]).
-export([op_check_pass/2, op_has_role/3, op_get_roles/1, op_add_role/4, op_del_role/3]).
-export([op_get_external_id/2, op_set_external_id/3, op_del_external_id/2]).
-export([config/0, parse/3, get/2, request/4, init/2, update/3, sync_op/3]).
-export([store_pass/1]).

-include_lib("nkactor/include/nkactor.hrl").
-include_lib("nkactor/include/nkactor_debug.hrl").
-include("nkactor_core.hrl").

-define(MAGIC_PASS, <<226,141,134,226,132,153,226,141,133>>). %%  "⍆ℙ⍅"/utf8
-define(INVALID_PASS_SLEEP, 250).
-define(LABEL_LOGIN, <<"login-user.core.netc.io">>).

%% ===================================================================
%% Types
%% ===================================================================

-type roles() :: [#{role:=binary(), namespace:=binary(), deep=>boolean()}].


%% ===================================================================
%% API
%% ===================================================================

%% @doc
-spec find_login(nkserver:id(), nkactor:namespace(), binary()) ->
    {ok, #actor_id{}} | {error, {login_unknown, binary}} | {error, term()}.

find_login(SrvId, Namespace, Id) ->
    case nkactor:find_cached_label(SrvId, Namespace, ?LABEL_LOGIN, Id) of
        {error, {label_not_found, _}} ->
            {error, {login_unknown, Id}};
        Other ->
            Other
    end.

%% @doc
has_role(Role, Namespace, Roles) ->
    do_has_role2(Roles, to_bin(Role), to_bin(Namespace), check).


%% @doc
write_pass(Pass, File) ->
    file:write_file(File, store_pass(Pass)).


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

op_has_role(UserId, Role, Namespace) ->
    nkactor:sync_op(UserId, {nkactor_has_role, to_bin(Role), to_bin(Namespace)}).


-spec op_get_roles(nkactor:id()) -> {ok, Member::nkactor:uid(), roles()} | {error, term()}.

op_get_roles(UserId) ->
    nkactor:sync_op(UserId, nkactor_get_roles).


op_add_role(UserId, Role, Namespace, Deep) when is_boolean(Deep)->
    nkactor:sync_op(UserId, {nkactor_add_role, to_bin(Role), to_bin(Namespace), Deep}).


op_del_role(UserId, Role, Namespace) ->
    nkactor:sync_op(UserId, {nkactor_del_role, to_bin(Role), to_bin(Namespace)}).


op_get_external_id(UserId, Name) ->
    nkactor:sync_op(UserId, {nkactor_get_external_id, to_bin(Name)}).

op_set_external_id(UserId, Name, Id) ->
    nkactor:sync_op(UserId, {nkactor_set_external_id, to_bin(Name), to_bin(Id)}).

op_del_external_id(UserId, Name) ->
    nkactor:sync_op(UserId, {nkactor_del_external_id, to_bin(Name)}).

%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_USERS,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list, update],
        short_names => [u],
        save_time => 5000,  % Used it tests
        fields_filter => [
            'spec.login',
            'spec.member'
        ],
        fields_sort => [
            'spec.login'
        ],
        fields_static => [
            'spec.login'
        ]
    }.


%% @doc
parse(_Op, _Actor, _Req) ->
    PassFun = fun(Pass) ->
        StoredPass = store_pass(Pass),
        {ok, StoredPass}
    end,
    Spec = #{
        login => text,
        member => binary,
        password => PassFun,
        roles => {list, #{
            role => text,
            namespace => binary,
            deep => boolean,
            '__mandatory' => [role],
            '__defaults' => #{namespace=><<>>, deep=>true}
        }},
        external_ids => map
    },
    {syntax, <<"v1a1">>, #{spec=>Spec}}.


%% @doc
request(get, <<"_rpc/checkpass">>, ActorId, Req) ->
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
init(create, #actor_st{actor = Actor}=ActorSt) ->
    {ok, ActorSt#actor_st{actor=add_label(Actor)}};

init(start, ActorSt) ->
    {ok, ActorSt}.


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
%% We don't really support updating the login through standard updated
update(Actor, _Opts, ActorSt) ->
    {ok, add_label(Actor), ActorSt}.


%% @doc
sync_op({nkactor_check_pass, Pass}, _From, ActorSt) ->
    #actor_st{actor=#{data:=Data}} = ActorSt,
    Valid = case Data of
        #{spec:=#{password:=Stored}} when Stored /= <<>> ->
            store_pass(Pass) == Stored;
        _ ->
            false
    end,
    Result =  {ok, Valid},
    {reply, Result, ActorSt};

sync_op(nkactor_get_roles, _From, #actor_st{actor=#{data:=#{spec:=Spec}}}=ActorSt) ->
    Member = maps:get(member, Spec, <<>>),
    Roles = maps:get(roles, Spec, []),
    {reply, {ok, Member, Roles}, ActorSt};

sync_op({nkactor_has_role, Role, Namespace}, _From, ActorSt) ->
    Reply = do_has_role(Role, Namespace, check, ActorSt),
    {reply, {ok, Reply}, ActorSt};

sync_op({nkactor_add_role, Role, Namespace, Deep}, _From, ActorSt) ->
    ActorSt2 = case do_has_role(Role, Namespace, Deep, ActorSt) of
        true ->
            ActorSt;
        false ->
            lager:error("NKLOG NO HAS1"),
            S3 = do_del_role(Role, Namespace, ActorSt),
            lager:error("NKLOG NO HAS2: ~p", [S3#actor_st.actor]),
            S4 = do_add_role(Role, Namespace, Deep, S3),
            lager:error("NKLOG NO HAS3: ~p", [S4#actor_st.actor]),
            S4
    end,
    {reply, ok, ActorSt2};

sync_op({nkactor_del_role, Role, Namespace}, _From, ActorSt) ->
    ActorSt2 = case do_has_role(Role, Namespace, any, ActorSt) of
        true ->
            lager:error("NKLOG T"),
            do_del_role(Role, Namespace, ActorSt);
        false ->
            lager:error("NKLOG F"),
            ActorSt
    end,
    {reply, ok, ActorSt2};

sync_op({nkactor_get_external_id, Name}, _From, ActorSt) ->
    #actor_st{actor=#{data:=#{spec:=Spec}}} = ActorSt,
    ExtIds = maps:get(external_ids, Spec, #{}),
    {reply, {ok, maps:get(Name, ExtIds)}, ActorSt};

sync_op({nkactor_set_external_id, Name, Value}, _From, ActorSt) ->
    #actor_st{actor=#{data:=#{spec:=Spec}=Data}=Actor} = ActorSt,
    ExtIds = maps:get(external_ids, Spec, #{}),
    case maps:put(Name, Value, ExtIds) of
        ExtIds ->
            {reply, ok, ActorSt};
        ExtIds2 ->
            Actor2 = Actor#{data:=Data#{spec:=Spec#{external_ids => ExtIds2}}},
            {reply_and_save, ok, ActorSt#actor_st{actor=Actor2}}
    end;

sync_op({nkactor_del_external_id, Name}, _From, ActorSt) ->
    #actor_st{actor=#{data:=#{spec:=Spec}=Data}=Actor} = ActorSt,
    ExtIds = maps:get(external_ids, Spec, #{}),
    case maps:remove(Name, ExtIds) of
        ExtIds ->
            {reply, ok, ActorSt};
        ExtIds2 ->
            Actor2 = Actor#{data:=Data#{spec:=Spec#{external_ids => ExtIds2}}},
            {reply_and_save, ok, ActorSt#actor_st{actor=Actor2}}
    end;

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
        _ when Pass == <<>> ->
            <<>>;
        _ ->
            Salt = <<"netcomposer">>,
            Iters = nkactor_core_app:get(pbkdf_iters),
            {ok, Pbkdf2} = pbkdf2:pbkdf2(sha, Pass2, Salt, Iters),
            Hash = nklib_util:lhash(Pbkdf2),
            <<?MAGIC_PASS/binary, Hash/binary>>
    end.


%% @private
add_label(#{data:=#{spec:=#{login:=Login}}}=Actor) ->
    Actor2 = nkactor_lib:rm_label_re(?LABEL_LOGIN, Actor),
    nkactor_lib:add_label(?LABEL_LOGIN, Login, Actor2);

add_label(ActorSt) ->
    ActorSt.


%% @private
do_has_role(Role, Namespace, Deep, #actor_st{actor=#{data:=#{spec:=Spec}}}) ->
    Roles = maps:get(roles, Spec, []),
    do_has_role2(Roles, Role, Namespace, Deep).


%% @private
do_has_role2([], _Role, _Ns2, _Deep2) ->
    false;

do_has_role2([#{role:=Role, namespace:=Ns, deep:=Deep}|Rest], Role, Ns2, Deep2) ->
    case {Ns, Deep, Deep2} of
        {Ns2, _, check} ->
            % Same role, same NS, we wanted to only to check
            true;
        {_, false, check} ->
            % Same role, another namespace, no deep
            do_has_role2(Rest, Role, Ns2, Deep2);
        {<<>>, true, check} ->
            % Base namespace, deep is true
            true;
        {_, true, check} ->
            % Different namespace, but deep. Let's see
            case binary:split(Ns2, Ns) of
                [_, <<>>] ->
                    % Ns was at the end
                    true;
                _ ->
                    do_has_role2(Rest, Role, Ns2, Deep2)
            end;
        {Ns2, Deep2, Deep2} ->
            % Same role, namespace and deep
            true;
        {Ns2, _, any} ->
            % Same role, namespace and we don't care deep
            true;
        _ ->
            lager:error("NKLOG NOTHING ~p ~p", [{Ns, Deep, Deep2}]),
            do_has_role2(Rest, Role, Ns2, Deep2)
    end;

do_has_role2([_|Rest], Role, Ns2, Deep2) ->
    do_has_role2(Rest, Role, Ns2, Deep2).



%% @private
do_del_role(Role, Namespace, #actor_st{actor=#{data:=#{spec:=Spec}=Data}=Actor}=ActorSt) ->
    Roles1 = maps:get(roles, Spec, []),
    Roles2 = do_del_role(Roles1, Role, Namespace, []),
    Actor2 = Actor#{data:=Data#{spec:=Spec#{roles=>Roles2}}},
    ActorSt#actor_st{actor=Actor2, is_dirty=true}.


%% @private
do_del_role([], _Role, _Ns, Acc) ->
    lists:reverse(Acc);

do_del_role([#{role:=Role, namespace:=Ns}|Rest], Role, Ns, Acc) ->
    do_del_role(Rest, Role, Ns, Acc);

do_del_role([RoleData|Rest], Role, Ns, Acc) ->
    do_del_role(Rest, Role, Ns, [RoleData|Acc]).


%% @private
do_add_role(Role, Namespace, Deep, #actor_st{actor=#{data:=#{spec:=Spec}=Data}=Actor}=ActorSt) ->
    Roles1 = maps:get(roles, Spec, []),
    Roles2 = [#{role=>Role, namespace=>Namespace, deep=>Deep}|Roles1],
    Actor2 = Actor#{data:=Data#{spec:=Spec#{roles=>Roles2}}},
    ActorSt#actor_st{actor=Actor2, is_dirty=true}.



%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).