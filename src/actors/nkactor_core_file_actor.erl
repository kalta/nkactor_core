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

%% @doc NkActor File Actor
-module(nkactor_core_file_actor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behavior(nkactor_actor).

-export([op_get_body/1, op_get_download_link/2, op_get_media/1]).
-export([config/0, parse/3, request/4, sync_op/3]).


-include_lib("nkactor/include/nkactor.hrl").
-include_lib("nkactor/include/nkactor_debug.hrl").
-include("nkactor_core.hrl").


%% ===================================================================
%% Operations
%% ===================================================================

%% @doc
op_get_body(Id) ->
    nkactor:sync_op(Id, nkactor_get_body, 60000).


%% @doc
op_get_download_link(Id, ExtUrl) ->
    nkactor:sync_op(Id, {nkactor_get_download_link, ExtUrl}).


op_get_media(Id) ->
    case nkactor:activate(Id) of
        {ok, #actor_id{group=?GROUP_CORE, resource=?RES_CORE_FILES}=ActorId, _} ->
            nkactor:sync_op(ActorId, nkactor_get_media);
        {ok, _} ->
            {error, actor_not_found};
        {error, Error} ->
            {error, Error}
    end.




%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

%% @doc
config() ->
    #{
        resource => ?RES_CORE_FILES,
        group => ?GROUP_CORE,
        versions => [<<"v1a1">>],
        verbs => [create, delete, deletecollection, get, list, update, upload],
        fields_filter => [
            'spec.name',
            'spec.size',
            'spec.content_type',
            'spec.external_id'
        ],
        fields_sort => [
            'spec.name',
            'spec.size',
            'spec.content_type'
        ],
        field_type => #{
            'spec.size' => integer
        },
        short_names => [],
        fields_static => [
            'spec.content_type',
            'spec.provider',
            'spec.external_id',
            'spec.size',
            'spec.hash',
            'spec.password'
        ]
    }.


%% @doc
parse(read, _Actor, _Req) ->
    Syntax = #{
        spec => #{
            content_type => binary,
            provider => binary,
            external_id => binary,
            size => integer,
            hash => binary,
            password => binary,
            '__mandatory' => [content_type, provider, size]
        },
        '__mandatory' => [spec]
    },
    {syntax, <<"v1a1">>, Syntax};

parse(create, Actor, Req) ->
    Syntax = #{
        spec => #{
            content_type => binary,
            provider => binary,
            external_id => binary,
            body_base64 => base64,
            body_binary => binary,
            url => binary
        },
        '__mandatory' => [spec]
    },
    case nkactor_lib:parse_actor_data(create, Actor, <<"v1a1">>, Syntax) of
        {ok, Actor2} ->
            #{data:=#{spec:=Spec2}}=Actor2,
            Params = maps:get(params, Req, #{}),
            % If the provider is valid, we will try to get the file and upload it
            % - if it is included in body (base64 or binary)
            % - if a url is provided, we try to download it
            % - if an external_id is provided, we try to get it
            do_parse_provider(Spec2, Params, Actor2, Req);
        {error, Error} ->
            {error, Error}
    end;

%% We allow fields in case they didn't change
%% fields_static makes sure no one is changed
parse(update, _Actor, _Req) ->
    Syntax = #{
        spec => #{
            content_type => binary,
            provider => binary,
            external_id => binary,
            hash => binary,
            size => integer,
            password => binary
        },
        '__mandatory' => [spec]
    },
    {syntax, <<"v1a1">>, Syntax}.


%% @doc
request(get, <<>>, ActorId, Req) ->
    case nkactor:get_actor(ActorId) of
        {ok, #{data:=Data}=Actor} ->
            Syntax = #{get_body_inline=>boolean},
            case nkactor_lib:parse_request_params(Req, Syntax) of
                {ok, #{get_body_inline:=true}} ->
                    case op_get_body(ActorId) of
                        {ok, _CT, Body} ->
                            #{spec:=Spec} = Data,
                            Body2 = base64:encode(Body),
                            Spec2 = Spec#{body_base64 => Body2},
                            {ok, Actor#{data:=Data#{spec:=Spec2}}};
                        {error, Error} ->
                            {error, Error}
                    end;
                {ok, _} ->
                    {ok, Actor};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

request(upload, <<>>, _ActorId, Req) ->
    Syntax = #{provider=>binary, '__mandatory'=>provider},
    case nkactor_lib:parse_request_params(Req, Syntax) of
        {ok, #{provider:=Provider}} ->
            case Req of
                #{
                    body := Body,
                    meta := #{nkactor_http_content_type:=CT}
                } ->
                    Body2 = #{
                        spec => #{
                            content_type => CT,
                            body_binary => Body,
                            provider => Provider
                        }
                    },
                    Req2 = Req#{
                        verb := create,
                        body := Body2
                    },
                    nkactor:request(Req2);
                _ ->
                    {error, request_body_invalid}
            end;
        {error, Error} ->
            {error, Error}
    end;

request(get, <<"_download">>, ActorId, _Req) ->
    case op_get_body(ActorId) of
        {ok, CT, Body} ->
            {raw, {CT, Body}};
        {error, Error} ->
            {error, Error}
    end;

request(get, <<"_rpc/download_link">>, ActorId, Req) ->
    ExtUrl = maps:get(external_url, Req, <<>>),
    case op_get_download_link(ActorId, ExtUrl) of
        {ok, Url, 0} ->
            {ok, #{url=>Url}};
        {ok, Url, TTL} ->
            {ok, #{url=>Url, ttl_secs=>TTL}};
        {error, Error} ->
            {error, Error}
    end;

request(_Verb, _Path, _ActorId, _Req) ->
    continue.


%% @doc
sync_op(nkactor_get_body, _From, ActorSt) ->
    #actor_st{srv=SrvId, actor=#{data:=Data}=Actor} = ActorSt,
    LinkType = ?LINK_CORE_FILE_PROVIDER,
    [ProviderUID] = nkactor_lib:get_linked_uids(LinkType, Actor),
    #{spec:=#{content_type:=CT, external_id:=Id}=Spec} = Data,
    FileMeta1 = #{
        name => Id,
        content_type => CT
    },
    FileMeta2 = case Spec of
        #{password:=Pass} ->
            FileMeta1#{password => Pass};
        _ ->
            FileMeta1
    end,
    FileMeta3 = case Spec of
        #{hash:=Hash} ->
            FileMeta2#{hash => Hash};
        _ ->
            FileMeta2
    end,
    Reply = case nkactor_core_file_provider_actor:op_get_spec(ProviderUID) of
        {ok, _ProvActorId, ProviderSpec} ->
            case nkfile:download(SrvId, ProviderSpec, FileMeta3) of
                {ok, Bin, _DownMeta} ->
                    {ok, CT, Bin};
                {error, file_not_found} ->
                    {error, {file_read_error, Id}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            ?ACTOR_LOG(warning, "file provider error: ~p", [Error]),
            {error, Error}
    end,
    {reply, Reply, ActorSt};

sync_op({nkactor_get_download_link, ExtUrl}, _From, ActorSt) ->
    #actor_st{actor_id=_ActorId, actor=#{data:=Data}=Actor} = ActorSt,
    #{spec:=#{external_id:=Id}} = Data,
    LinkType = ?LINK_CORE_FILE_PROVIDER,
    [ProvUID] = nkactor_lib:get_linked_uids(LinkType, Actor),
    Reply = case nkactor_core_file_provider_actor:op_get_direct_download_link(ProvUID, Id) of
        {ok, Link, TTL} ->
            {ok, Link, TTL};
        {error, {storage_class_incompatible, _}} ->
            %{ok, <<Url/binary, "/_download">>, 0}
            {error, no_public_address};
        {error, Error} ->
            {error, Error}
    end,
    {reply, Reply, ActorSt};

sync_op(nkactor_get_media, _From, ActorSt) ->
    #actor_st{actor_id=#actor_id{uid=UID, name=Name}=ActorId, actor=Actor} = ActorSt,
    #{data:=#{spec:=#{content_type:=CT, size:=Size}}} = Actor,
    Media = #{
        file_id => UID,
        content_type => CT,
        size => Size,
        name => Name
    },
    {reply, {ok, ActorId, Media}, ActorSt};

sync_op(_Op, _From, _ActorSt) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
% If provider in params, move to spec
do_parse_provider(Spec, #{provider:=Provider}, Actor, Req) ->
    do_parse_provider(Spec#{provider=>Provider}, #{}, Actor, Req);

do_parse_provider(#{provider:=ProviderId}=Spec, _Params, Actor, Req) ->
    case nkactor_lib:add_checked_link(ProviderId, ?GROUP_CORE, ?RES_CORE_FILE_PROVIDERS, Actor, ?LINK_CORE_FILE_PROVIDER) of
        {ok, ProvActorId, Actor2} ->
            case nkactor_core_file_provider_actor:op_get_spec(ProviderId) of
                {ok, _, ProvSpec} ->
                    % We have a valid provider,
                    do_parse_upload(Spec, ProvActorId, ProvSpec, Actor2, Req);
                {error, _} ->
                    {error, {provider_unknown, ProviderId}}
            end;
        {error, _} ->
            {error, {provider_unknown, ProviderId}}
    end;

do_parse_provider(_Spec, _Params, _Actor, _Req) ->
    {error, {field_missing, <<"data.spec.provider">>}}.


%% @private
do_parse_upload(#{body_base64:=Bin}=Spec, ProvActorId, ProvSpec, Actor, Req) ->
    Spec2 = maps:remove(body_base64, Spec),
    do_parse_upload(Spec2#{body_binary=>Bin}, ProvActorId, ProvSpec, Actor, Req);

do_parse_upload(#{url:=Url}=Spec, ProvActorId, ProvSpec, Actor, Req) ->
    ?ACTOR_LOG(notice, "resolving url: ~s", [Url]),
    case get_url(ProvActorId, ProvSpec, Url) of
        {ok, CT, Body} ->
            Spec2 = maps:remove(url, Spec),
            Spec3 = Spec2#{body_binary=>Body, content_type=>CT},
            do_parse_upload(Spec3, ProvActorId, ProvSpec, Actor, Req);
        {error, Error} ->
            {error, Error}
    end;

do_parse_upload(#{body_binary:=Bin, content_type:=CT}=Spec, _ProvActorId, ProvSpec, Actor, Req) ->
    #{uid:=UID, data:=Data} = Actor,
    FileMeta1 = #{name => UID, content_type => CT},
    #{srv:=SrvId} = Req,
    ?ACTOR_LOG(notice, "starting upload", []),
    case nkfile:upload(SrvId, ProvSpec, FileMeta1, Bin) of
        {ok, FileMeta2, _UpMeta} ->
            #{size:=Size} = FileMeta2,
            Spec1 = maps:remove(body_binary, Spec),
            Spec2 = Spec1#{
                size => Size,
                external_id => UID
            },
            Spec3 = case FileMeta2 of
                #{password:=Pass} ->
                    Spec2#{password => Pass};
                _ ->
                    Spec2
            end,
            Spec4 = case FileMeta2 of
                #{hash:=Hash} ->
                    Spec3#{hash => Hash};
                _ ->
                    Spec3
            end,
            Data2 = Data#{spec:=Spec4},
            {ok, Actor#{data:=Data2}};
        {error, Error} ->
            {error, Error}
    end;

do_parse_upload(#{body_binary:=_}, _ProvActorId, _ProvSpec, _Actor, _Req) ->
    {error, {field_missing, <<"spec.content_type">>}};

do_parse_upload(#{external_id:=Id}=Spec, _ProvActorId, ProvSpec, Actor, Req) ->
    #{srv:=SrvId} = Req,
    case nkfile:get_file_meta(SrvId, ProvSpec, Id) of
        {ok, #{content_type:=CT, size:=Size}} ->
            #{data:=Data} = Actor,
            Data2 = Data#{spec => Spec#{size => Size, content_type=>CT}},
            {ok, Actor#{data:=Data2}};
        %{ok, _} ->
        %    {error, content_type_invalid};
        {error, {file_too_large, Id}} ->
            case nkfile:delete(SrvId, ProvSpec, #{name=>Id}) of
                ok ->
                    ?ACTOR_LOG(notice, "deleted file too large: ~p", [Id]);
                {error, Error} ->
                    ?ACTOR_LOG(warning, "could not delete file too large ~p: ~p", [Id, Error])
            end,
            {error, {file_too_large, Id}};
        {error, Error} ->
            {error, Error}
    end;

%%do_parse_upload(#{external_id:=_}, _ProvActorId, _ProvSpec, _Actor, _Req) ->
%%    {error, {field_missing, <<"spec.content_type">>}};

do_parse_upload(_Spec, _ProvActorId, _ProvSpec, _Actor, _Req) ->
    {error, {field_missing, <<"spec.body_base64">>}}.


%% @doc
-spec get_url(#actor_id{}, nkfile:provider_spec(), binary()) ->
    {ok, CT::binary(), Body::binary()} | {error, term()}.

get_url(ProvActorId, ProviderSpec, Url) ->
    #actor_id{namespace=Namespace} = ProvActorId,
    MaxSize = maps:get(max_size, ProviderSpec, 0),
    Opts = [
        follow_redirect,
        {pool, Namespace}
    ],
    case catch hackney:request(get, Url, [], <<>>, Opts) of
        {ok, 200, Hds, Ref} ->
            case hackney_headers:parse(<<"content-length">>, Hds) of
                Size when is_integer(Size) andalso (MaxSize==0 orelse Size =< MaxSize) ->
                    case hackney_headers:parse(<<"content-type">>, Hds) of
                        {T1, T2, _} ->
                            case hackney:body(Ref) of
                                {ok, Body} ->
                                    CT = <<T1/binary, $/, T2/binary>>,
                                    {ok, CT, Body};
                                {error, Error} ->
                                    {error, {hackney_error, Error}}
                            end;
                        _ ->
                            {error, {file_read_error, Url}}
                    end;
                _ ->
                    {error, {file_too_large, Url}}
            end;
        _ ->
            {error, {file_read_error, Url}}
    end.

%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).