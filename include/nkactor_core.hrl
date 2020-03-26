-ifndef(NKACTOR_CORE_HRL_).
-define(NKACTOR_CORE_HRL_, 1).

%% ===================================================================
%% Defines
%% ===================================================================


-define(GROUP_CORE, <<"core">>).
-define(GROUP_CORE_API_V1A1, <<"v1a1">>).

-define(RES_CORE_HTTP_POOOLER, <<"httppooler">>).
-define(RES_CORE_ACCESS_IDS, <<"accessids">>).
-define(RES_CORE_EVENTS, <<"events">>).
-define(RES_CORE_USERS, <<"users">>).
-define(RES_CORE_CONTACTS, <<"contacts">>).
-define(RES_CORE_TOKENS, <<"tokens">>).
-define(RES_CORE_CONFIGMAPS, <<"configmaps">>).
-define(RES_CORE_TASKS, <<"tasks">>).
-define(RES_CORE_CRON_JOBS, <<"cronjobs">>).
-define(RES_CORE_SESSIONS, <<"sessions">>).
-define(RES_CORE_FILES, <<"files">>).
-define(RES_CORE_FILE_PROVIDERS, <<"fileproviders">>).
-define(RES_CORE_NODES, <<"nodes">>).


-define(LINK_TARGET_CRONJOB, <<"target-cronjobs.core.netc.io">>).

-define(LINK_MEMBER_USER, <<"member-user.core.netc.io">>).

-define(LINK_CORE_FILE_PROVIDER, <<"file-provider-files.core.netc.io">>).
-define(LINK_CORE_CONTACT_USER, <<"user-contacts.core.netc.io">>).

-define(LABEL_ACCESS_ID, <<"id-access_ids.core.netc.io">>).

-endif.