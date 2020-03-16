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
%% ------------------------------------------------------------------

%% @doc
-module(nkactor_core_test_3).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-import(nkactor_core_test_util, [
    req/1, kapi_req/1, api_watch/1, wait_api_event/2, api_watch_stop/1,
    http_get/1, http_post/2, http_put/2,
    http_delete/1, http_list/1, http_watch/1, wait_http_event/2, http_watch_stop/1,
    clean_events/0, yaml/1]).

-compile(export_all).
-compile(nowarn_export_all).



%% ===================================================================
%% Public
%% ===================================================================


% export MINIO_ACCESS_KEY=5UBED0Q9FB7MFZ5EWIOJ; export MINIO_SECRET_KEY=CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI; minio server .
% create bucket1
% Use create_test_data before
file_test_s3() ->
    kapi_req(#{verb=>deletecollection, namespace=>"a.test.my_actors", resource=>files}),
    kapi_req(#{verb=>deletecollection, namespace=>"a.test.my_actors", resource=>fileproviders}),

    SFP = <<"
        kind: FileProvider
        spec:
            storageClass: nkfile_s3
            encryptionAlgo: aes_cfb128
            maxSize: 3
            directDownload: true
            directDownloadSecs: 3
            directUpload: true
            directUploadSecs: 3
            s3Config:
                scheme: http
                host: localhost
                port: 9000
                key: '5UBED0Q9FB7MFZ5EWIOJ'
                secret: 'CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI'
                bucket: bucket1
                path: 'a/b'
        metadata:
            name: fs3
            namespace: a.test.my_actors
    ">>,

    % Cannot get and uploadLink if we have encryption or hash
    {created, _FP2} = kapi_req(#{verb=>create, body=>yaml(SFP)}),
    {400, #{ <<"reason">> :=  <<"storage_class_incompatible">>}} = http_get("/namespaces/a.test.my_actors/fileproviders/fs3/_rpc/uploadLink?contentType=ct1"),

    % Re-create the fileprovider without encryption
    {200, _} = http_delete("/namespaces/a.test.my_actors/fileproviders/fs3"),
    SFP2 = re:replace(SFP, <<"encryptionAlgo:">>, <<"#encryptionAlgo:">>, [{return, binary}]),
    {created, _} = kapi_req(#{verb=>create, body=>yaml(SFP2)}),
    % Get an upload link (Url1, Id1) for a file with content-type ct1

    {200, L1} = http_get("/namespaces/a.test.my_actors/fileproviders/fs3/_rpc/uploadLink?contentType=ct1"),
    #{
        <<"kind">> := <<"UploadLink">>,
        <<"data">> := #{
            <<"method">>:=<<"PUT">>, <<"url">>:=Url1, <<"id">>:=Id1, <<"ttlSecs">>:=3
        }
    } = L1,
    {_, 13} = binary:match(Url1, <<"/bucket1/a/b/">>),

    % Upload a file too large
    {ok, 200, _, _Body} = hackney:request(<<"PUT">>, Url1, [{<<"content-type">>, <<"ct1">>}], <<"1234">>, [with_body]),

    % Try to create the file using the uploaded
    % (the file will be deleted on S3)
    F1 = <<"
        spec:
            contentType: ct2
            externalId: ", Id1/binary, "
            provider: fs3.a.test.my_actors
        metadata:
            name: file3
    ">>,
    {400, #{<<"reason">>:=<<"file_too_large">>}} = http_post("/namespaces/a.test.my_actors/files", yaml(F1)),

    % Upload a new file (with content type ct1) , and then create the file object. Content-Type must match.
    {ok, 200, _, _Body} = hackney:request(<<"PUT">>, Url1, [{<<"content-type">>, <<"ct1">>}], <<"123">>, [with_body]),
    {400, #{<<"reason">>:=<<"content_type_invalid">>}} = http_post("/namespaces/a.test.my_actors/files", yaml(F1)),


    F1b = re:replace(F1, <<"ct2">>, <<"ct1">>, [{return, binary}]),
    % Different format for provider
    F1c = re:replace(F1b, <<"fs3.a.test.my_actors">>, <<"core:fileproviders:fs3.a.test.my_actors">>, [{return, binary}]),
    {201, F3} = http_post("/namespaces/a.test.my_actors/files", yaml(F1c)),
    #{
        <<"spec">> := #{
            <<"externalId">> := Id1,
            <<"provider">> := <<"core:fileproviders:fs3.a.test.my_actors">>,
            <<"contentType">> := <<"ct1">>,
            <<"size">> := 3
        } = SpecF3
    } = F3,
    false = maps:is_key(<<"bodyBase64">>, SpecF3),
    {200, F3} = http_get("/namespaces/a.test.my_actors/files/file3"),
    {200, F4} = http_get("/namespaces/a.test.my_actors/files/file3?getBodyInline=true"),
    #{<<"spec">>:=#{<<"bodyBase64">>:=Base64Body}} =F4,
    Base64Body = base64:encode(<<"123">>),
    {ok, {{_, 200, _}, Hds1, "123"}} = nkactor_core_test_util:httpc("/namespaces/a.test.my_actors/files/file3/_download"),
    "ct1" = nklib_util:get_value("content-type", Hds1),

    % Get a download link
    {200, DL1} = nkactor_core_test_util:http_get("/namespaces/a.test.my_actors/files/file3/_rpc/downloadLink"),
    #{
        <<"kind">> := <<"DownloadLink">>,
        <<"data">> := #{<<"url">>:=Url2, <<"ttlSecs">>:=3}
    } = DL1,
    {ok, {{_, 200, _}, Hds2, "123"}} = httpc:request(binary_to_list(Url2)),
    "ct1" = nklib_util:get_value("content-type", Hds2),

    % Links should have been expired
    timer:sleep(4100),
    {ok, 403, _, Exp1} = hackney:request(<<"PUT">>, Url1, [{<<"content-type">>, <<"ct1">>}], <<"432">>, [with_body]),
    {_, 19} = binary:match(Exp1, <<"Request has expired">>),
    {ok, 403, _, Exp2} = hackney:request(get, Url2, [], <<>>, [with_body]),
    {_, 19} = binary:match(Exp2, <<"Request has expired">>),
    ok.



cb_event() ->
    req(#{verb=>deletecollection, namespace=>"a.test.my_actors", resource=>sessions}),

    SFP = <<"
        kind: Session
        spec:
            ttlSecs: 120
        metadata:
            name: sess1
            callbackUrl: http://127.0.0.1:9001/_test
            namespace: a.test.my_actors
    ">>,
    {created, _} = req(#{verb=>create, body=>yaml(SFP)}),
    ok.












