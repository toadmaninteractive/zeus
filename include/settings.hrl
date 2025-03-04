-define(WEB_APP, zeus).
-define(WEB_ADDR, {127, 0, 0, 1}).
-define(WEB_PORT, 4444).
-define(ACTIVE_THEME,  <<"amsterdam">>).

-define(SESSION_ID_VAR, <<"sid">>).
-define(SESSION_LIFETIME, 86400).   % 1 day
-define(SESSION_REFRESH, 60).       % 1 min

-define(ZEUS_INSTANCES, zeus_instances).
-define(ZEUS_USERS, zeus_users).
-define(ZEUS_REALMS, zeus_realms).
-define(ZEUS_ROLES, zeus_roles).
-define(ZEUS_LOGS, zeus_logs).

-record(route, {
    path = <<>> :: binary(),
    authenticated = false :: boolean(),
    session_id :: binary() | 'undefined'
}).

-record(api_req, {
    method :: atom(),
    path :: binary(),
    path_parts :: [binary()],
    pathinfo :: binary() | 'undefined',
    authenticated = false :: boolean(),
    session_id :: binary() | 'undefined',
    qs = <<>> :: binary() | 'undefined',
    qs_vals = [] :: list() | 'undefined',
    json :: jsx:json_term() | 'undefined'
}).

-record(validation_entry, {
    param :: binary(),
    validator :: atom(),
    kind :: 'binary' | 'integer' | 'float',
    value :: term(),
    valid = false :: boolean()
}).

-type api_req() :: #api_req{}.

-record(zeus_log, {
    id :: binary() | '_',
    timestamp :: non_neg_integer() | '_',
    datetime :: binary() | '_',
    instance_id :: binary() | '_',
    username :: binary() | '_',
    realm :: atom() | '_',
    http_method :: atom() | '_',
    url :: binary() | '_',
    qs :: binary() | '_',
    body :: term() | '_',
    agent_data :: term() | '_',
    agent_message :: binary() | '_'
}).

-type zeus_log() :: #zeus_log{}.

-define(ws_code_not_authorized, 4403).
-define(ws_code_not_found, 4404).
