-module(zeus).

%% Include files

-include("settings.hrl").
-include("zeus_server.hrl").

%% Exported functions

-export([
    start/0,
    stop/0,
    init/0,
    reset/0
]).

-export_type([
    binding/0,
    json_binding/0
]).

-type binding() :: {'binding', [atom()] | atom()} | {'binding', [atom()] | atom(), binary()} | json:json() | atom().
-type json_binding() :: binding() | number() | binary() | boolean() | 'null' | [json_binding()] | {[{binary(), json_binding()}]}.

%% API

-spec start() -> 'ok' | {'error', Reason :: term()}.

start() ->
    aplib:start_app_recursive(zeus).

-spec stop() -> 'ok'.

stop() ->
    application:stop(zeus).

-spec init() -> 'ok'.

init() ->
    mnesia:start(),
    Tables = [?ZEUS_INSTANCES, ?ZEUS_USERS, ?ZEUS_REALMS, ?ZEUS_ROLES, ?ZEUS_LOGS],
    [mnesia:delete_table(Table) || Table <- Tables],
    mnesia:create_table(?ZEUS_INSTANCES, [{type, set}, {disc_copies, [node()]}, {record_name, zeus_instance}, {attributes, record_info(fields, zeus_instance)}]),
    mnesia:create_table(?ZEUS_USERS, [{type, set}, {disc_copies, [node()]}, {record_name, zeus_user}, {attributes, record_info(fields, zeus_user)}]),
    mnesia:create_table(?ZEUS_REALMS, [{type, set}, {disc_copies, [node()]}, {record_name, zeus_realm}, {attributes, record_info(fields, zeus_realm)}]),
    mnesia:create_table(?ZEUS_ROLES, [{type, set}, {disc_copies, [node()]}, {record_name, zeus_role}, {attributes, record_info(fields, zeus_role)}]),
    mnesia:create_table(?ZEUS_LOGS, [{type, set}, {disc_copies, [node()]}, {record_name, zeus_log}, {attributes, record_info(fields, zeus_log)}]),
    ok.

-spec reset() -> 'ok'.

reset() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    init(),
    ok.

%% Local functions
