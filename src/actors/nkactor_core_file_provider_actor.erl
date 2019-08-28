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

%% @doc NkActor File Provider Actor
%%
%% spec
%% ----


-module(nkactor_core_file_provider_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([op_get_spec/1, op_get_direct_download_link/2, op_get_upload_link/2,
         op_get_file_meta/2]).
%%-export([link_to_provider/3]).
-export([config/0, parse/2, request/4, init/2, update/2, sync_op/3]).
-export_type([run_state/0]).


-include_lib("nkactor/include/nkactor.hrl").
-include_lib("nkactor/include/nkactor_debug.hrl").
-include("nkactor_core.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type run_state() :: #{
       provider_spec := nkfile:provider_spec()
    }.


%% ===================================================================
%% Operations
%% ===================================================================

%% @doc
-spec op_get_spec(nkactor:id()) ->
    {ok, #actor_id{}, map()} | {error, term()}.

op_get_spec(Id) ->
    nkactor_srv:sync_op(Id, nkactor_get_spec).


%% @doc
op_get_direct_download_link(Id, ExternalId) ->
    nkactor_srv:sync_op(Id, {nkactor_get_direct_download_link, ExternalId}).


%% @doc
op_get_upload_link(Id, CT) ->
    nkactor_srv:sync_op(Id, {nkactor_get_upload_link, CT}).


%% @doc
op_get_file_meta(Id, ExternalId) ->
    nkactor_srv:sync_op(Id, {nkactor_get_file_meta, ExternalId}).


%%
%%%% @private
%%link_to_provider(SrvId, ApiId, Actor) ->
%%    ProviderPath = nkactor_lib:process_id(SrvId, ApiId),
%%    case nkservice_actor:activate({SrvId, ProviderPath}) of
%%        {ok, #actor_id{group=?GROUP_CORE, resource=?RES_CORE_FILE_PROVIDERS}=ProvActorId, _} ->
%%            LinkType = nkactor_core_actor_util:link_type(?GROUP_CORE, ?LINK_CORE_FILE_PROVIDER),
%%            {ok, nkactor_core_actor_util:add_link(ProvActorId, LinkType, Actor)};
%%        _ ->
%%            {error, {provider_invalid, ApiId}}
%%    end.


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_FILE_PROVIDERS,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list, patch, update, watch, upload],
        short_names => [],
        camel => <<"FileProvider">>,
        permanent => true,
        fields_filter => [
            'spec.storage_class'
        ],
        fields_sort => [
            'spec.storage_class'
        ],
        fields_static => [
            'spec.storage_class',
            'spec.encryption_algo',
            'spec.hash_algo',
            'spec.s3_config.bucket',
            'spec.s3_config.path',
            'spec.filesystem_config.file_path'
        ]
    }.


%% @doc
parse(Actor, _Req) ->
    Syntax = #{
        spec => #{
            storage_class => {atom, [nkfile_filesystem, nkfile_s3]},
            max_size => {integer, 0, none},
            encryption_algo => {atom, [aes_cfb128]},
            hash_algo => {atom, [sha256]},
            direct_download => boolean,
            direct_upload => boolean,
            direct_download_secs => pos_integer,
            direct_upload_secs => pos_integer,
            filesystem_config => #{
                file_path => binary,
                '__mandatory' => [file_path]
            },
            s3_config => #{
                region => binary,
                key => binary,
                secret => binary,
                bucket => binary,
                path => binary,
                url => binary,                        %% Use url or scheme/host/port
                scheme => {atom, [http, https]},
                host => binary,
                port => integer,
                '__mandatory' => [key, secret, bucket]
            },
            '__mandatory' => [storage_class]
        },
        '__mandatory' => [spec]
    },
    case nkactor_lib:parse_actor_data(Actor, <<"v1a1">>, Syntax) of
        {ok, #{data:=#{spec:=#{storage_class:=Class}=Spec2}}=Actor2} ->
            case Class of
                nkfile_filesystem ->
                    case maps:is_key(filesystem_config, Spec2) of
                        true ->
                            {ok, Actor2};
                        false ->
                            {error, {field_missing, <<"data.spec.filesystem_config">>}}
                    end;
                nkfile_s3 ->
                    case maps:is_key(s3_config, Spec2) of
                        true ->
                            {ok, Actor2};
                        false ->
                            {error, {field_missing, <<"data.spec.s3_config">>}}
                    end
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
%% Redirect to files, adding parameter
request(Verb, <<"files/", Rest/binary>>, ActorId, Req) when Verb==create; Verb==upload ->
    #{params:=Params}=Req,
    Path = nkactor_lib:actor_id_to_path(ActorId),
    Req2 = Req#{
        resource := ?RES_CORE_FILES,
        subresource := Rest,
        params := Params#{provider => Path}
    },
    Req3 = maps:remove(name, Req2),
    nkactor_request:request(Req3);

request(get, <<"_rpc/upload_link">>, ActorId, #{params:=Params}) ->
    Syntax = #{content_type => binary},
    case nklib_syntax:parse(Params, Syntax) of
        {ok, #{content_type:=CT}, _} ->
            case op_get_upload_link(ActorId, CT) of
                {ok, Method, Url, Name, TTL} ->
                    {ok, #{method=>Method, url=>Url, id=>Name, ttl_secs=>TTL}};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, {field_missing, <<"content_type">>}}
    end;

request(_Verb, _Path, _ActorId, _Req) ->
    continue.


%%%% @doc
%%make_external(_SrvId, #actor{data=Data}=Actor, _Vsn) ->
%%    % This function can be called with 'fake' empty actors
%%    Spec = maps:get(spec, Data, #{}),
%%    case maps:find(<<"s3_config">>, Spec) of
%%        {ok, SpecS3} ->
%%            SpecS3B = SpecS3#{<<"secret">>=><<>>},
%%            Spec2 = Spec#{<<"s3_config">>:=SpecS3B},
%%            {ok, Actor#actor{data=Data#{spec:=Spec2}}};
%%        _ ->
%%            {ok, Actor}
%%    end.


%% @doc
init(_Op, #actor_st{actor=Actor}=ActorSt) ->
    set_spec_cache(Actor, ActorSt).


%% @doc
update(NewActor, ActorSt) ->
    case set_spec_cache(NewActor, ActorSt) of
        {ok, ActorSt2} ->
            {ok, NewActor, ActorSt2};
        {error, Error} ->
            {error, Error, ActorSt}
    end.


%% @doc
sync_op(nkactor_get_spec, _From, #actor_st{actor_id=ActorId, run_state=RunState}=ActorSt) ->
    #{provider_spec:=Spec} = RunState,
    {reply, {ok, ActorId, Spec}, ActorSt};

sync_op({nkactor_get_upload_link, CT}, _From, #actor_st{srv=SrvId, run_state=RunState}=ActorSt) ->
    #{provider_spec:=Spec} = RunState,
    Name = nklib_util:luid(),
    FileMeta = #{name=>Name, content_type=>nklib_util:to_binary(CT)},
    case nkfile:make_upload_link(SrvId, Spec, FileMeta) of
        {ok, Verb, Url, TTL} ->
            {reply, {ok, Verb, Url, Name, TTL}, ActorSt};
        {error, Error} ->
            {reply, {error, Error}, ActorSt}
    end;

sync_op({nkactor_get_direct_download_link, Id}, _From, #actor_st{srv=SrvId, run_state=RunState}=ActorSt) ->
    #{provider_spec:=Spec} = RunState,
    FileMeta = #{name=>Id},
    case nkfile:make_download_link(SrvId, Spec, FileMeta) of
        {ok, <<"GET">>, Url, TTL} ->
            {reply, {ok, Url, TTL}, ActorSt};
        {error, Error} ->
            {reply, {error, Error}, ActorSt}
    end;

sync_op({nkactor_get_file_meta, Id}, _From, #actor_st{srv=SrvId, run_state=RunState}=ActorSt) ->
    #{provider_spec:=Spec} = RunState,
    case nkfile:get_file_meta(SrvId, Spec, Id) of
        {ok, Meta} ->
            {reply, {ok, Meta}, ActorSt};
        {error, {file_too_large, Id}} ->
            case nkfile:delete(SrvId, Spec, #{name=>Id}) of
                ok ->
                    ?ACTOR_LOG(notice, "deleted file too large: ~p", [Id], ActorSt);
                {error, Error} ->
                    ?ACTOR_LOG(warning, "could not delete file too large ~p: ~p", [Id, Error], ActorSt)
            end,
            {reply, {error, {file_too_large, Id}}, ActorSt};
        {error, Error} ->
            {reply, {error, Error}, ActorSt}
    end;

sync_op(_Op, _From, _ActorSt) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
set_spec_cache(#{data:=#{spec:=Spec}}, ActorSt) ->
    #actor_st{srv=SrvId, actor_id=ActorId, run_state=RunState} = ActorSt,
    #actor_id{uid=UID} = ActorId,
    Spec2 = Spec#{id => UID},
    case nkfile:parse_provider_spec(SrvId, Spec2) of
        {ok, Spec3} ->
            RunState1 = case RunState of
                undefined ->
                    #{};
                _ ->
                    RunState
            end,
            RunState2 = RunState1#{provider_spec => Spec3},
            {ok, ActorSt#actor_st{run_state=RunState2}};
        {error, Error} ->
            ?ACTOR_LOG(warning, "could not parse provider spec: ~p: ~p", [Spec2, Error]),
            {error,provider_spec_invalid}
    end.



%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).