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
-module(nkactor_core_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([update_roles/1]).
-export([create_linked_user/1, update_linked_user_password/3, update_linked_user_password/2,
         update_linked_user_login/2, delete_linked_user/2]).

-include_lib("nkactor/include/nkactor.hrl").
-include_lib("nkactor/include/nkactor_debug.hrl").
-include("nkactor_core.hrl").


%% ===================================================================
%% Linked User API
%%
%% Allows to develop actor that are linked to an user
%% The actor must have the fields:



%% ===================================================================

update_roles([]) ->
    ok;

update_roles([Role|Rest]) ->
    Syntax = #{
        uid => binary,
        namespace => binary,
        password => binary,
        roles => {list, #{
            role => binary,
            namespace => binary,
            deep => binary,
            '__mandatory' => [role],
            '__defaults' => #{namespace => <<>>, deep=>false}
        }},
        '__mandatory' => [uid, namespace, password, roles]
    },
    case nklib_syntax:parse(Role, Syntax) of
        {ok, #{uid:=UID, namespace:=Ns, password:=Pass, roles:=Roles}, _} ->
            User = #{
                group => ?GROUP_CORE,
                resource => ?RES_CORE_USERS,
                namespace => Ns,
                data => #{
                    spec => #{
                        password => Pass,
                        roles => Roles
                    }
                }
            },
            case nkactor:update(UID, User, #{}) of
                {error, actor_not_found} ->
                    case nkactor:create(User, #{forced_uid=>UID}) of
                        {ok, _} ->
                            ok;
                        {error, Error} ->
                            lager:error("Could not create role ~s: ~p", [UID, Error])
                    end;
                {ok, _} ->
                    ok
            end,
            update_roles(Rest);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Creates an user linked to another actor
%% Actor must use the fields:
%% - spec.login
%% - status.user_namespace (optional)
%% The user is created with same login and member pointing to calling actor
%% - The name of user is a hash of the login
%% - Field 'user_uid' is added to caller status field
%% - The caller actor is linked to the created user
create_linked_user(Actor) ->
    case Actor of
        #{data:=#{spec:=#{login:=Login}}=Data} when Login /= <<>> ->
            #{uid := UID, namespace := ActorNamespace} = Actor,
            Name = nklib_util:to_lower(nklib_util:lhash(Login)),
            Status = maps:get(status, Data, #{}),
            Namespace = maps:get(user_namespace, Status, ActorNamespace),
            UserActor = #{
                group => ?GROUP_CORE,
                resource => ?RES_CORE_USERS,
                namespace => Namespace,
                name => Name,
                data => #{
                    spec => #{
                        login => Login,
                        member => UID,
                        password => <<>>
                    }
                }
            },
            case nkactor:create(UserActor, #{}) of
                {ok, #actor_id{uid=UserUID}} ->
                    Actor2 = Actor#{
                        data:=Data#{status => Status#{user_uid => UserUID}}
                    },
                    Actor3 = nkactor_lib:rm_links(Actor2, ?LINK_MEMBER_USER),
                    Actor4 = nkactor_lib:add_link(UserUID, Actor3, ?LINK_MEMBER_USER),
                    {ok, Actor4};
                {error, actor_already_exists} ->
                    {error, {login_exists, Login}};
                {error, Error} ->
                    {error, {user_creation, Error}}
            end;
        _ ->
            {ok, Actor}
    end.


%% @doc Updates a linked user's password, checking old first
update_linked_user_password(OldPass, NewPass, Actor) ->
    case Actor of
        #{data:=#{status:=#{user_uid:=UserUID}}} ->
            case nkactor_core_user_actor:op_check_pass(UserUID, OldPass) of
                {ok, true} ->
                    update_linked_user_password(NewPass, Actor);
                {ok, false} ->
                    {error, password_invalid};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, user_not_linked}
    end.


%% @doc Updates a linked user's password
update_linked_user_password(Pass, Actor) ->
    case Actor of
        #{data:=#{status:=#{user_uid:=UserUID}}} ->
            Update = #{password => Pass},
            case nkactor:update(UserUID, #{data=>#{spec=>Update}}, #{merge_data=>true}) of
                {ok, _} ->
                    ok;
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, user_not_linked}
    end.


%% @doc
update_linked_user_login(NewActor, #actor_st{actor=OldActor}=ActorSt) ->
    #{data:=#{spec:=NewSpec}} = NewActor,
    #{data:=#{spec:=OldSpec}} = OldActor,
    OldLogin = maps:get(login, OldSpec, <<>>),
    NewLogin = maps:get(login, NewSpec, OldLogin),
    case NewLogin == OldLogin of
        true ->
            {ok, ActorSt#actor_st{actor=NewActor}};
        false ->
            ActorSt3 = case delete_linked_user(NewActor, ActorSt) of
                {ok, ActorSt2} ->
                    ActorSt2;
                {error, DeleteError} ->
                    ?ACTOR_LOG(warning, "could not delete old linked user ~s: ~p",
                                [OldLogin, DeleteError], ActorSt),
                    ActorSt
            end,
            case NewLogin of
                <<>> ->
                    {ok, ActorSt3};
                _ ->
                    #actor_st{actor=Actor3} = ActorSt3,
                    case create_linked_user(Actor3) of
                        {ok, Actor4} ->
                            {ok, ActorSt3#actor_st{actor=Actor4}};
                        {error, CreateError} ->
                            {error, CreateError, ActorSt}
                    end
            end
    end.



%% @doc Deletes a linked user.
%% The link must be removed from current actor, and then it must be saved on db,
%% after that the user can be removed
%% If NewAc

delete_linked_user(Actor, #actor_st{actor=OldActor}=ActorSt) ->
    % Use the old actor to delete the old user
    case OldActor of
        #{data:=#{status:=#{user_uid:=UserUID}}} ->
            Actor2 = nkactor_lib:rm_links(Actor, ?LINK_MEMBER_USER),
            Actor3 = case Actor2 of
                #{data:=#{status:=Status}=Data} ->
                    Status2 = maps:remove(user_uid, Status),
                    Actor2#{data:=Data#{status:=Status2}};
                _ ->
                    Actor2
            end,
            ActorSt2 = ActorSt#actor_st{actor=Actor3, is_dirty=true},
            case nkactor_srv_lib:save(remove_link, ActorSt2) of
                {ok, ActorSt3} ->
                    case nkactor:delete(UserUID) of
                        ok ->
                            ok;
                        {error, Error} ->
                            ?ACTOR_LOG(warning, "could not delete external user ~s (~p)", [UserUID, Error])
                    end,
                    {ok, ActorSt3};
                {{error, Error}, _} ->
                    ?ACTOR_LOG(warning, "could not update externaluser ~s (~p)", [UserUID, Error]),
                    {error, Error}
            end;
        _ ->
            {ok, ActorSt#actor_st{actor=Actor}}
    end.

