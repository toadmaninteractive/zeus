-module(zeus_session).

-behaviour(gen_server).

%% Include files

-include("settings.hrl").
-include("zeus_server.hrl").

%% Exported functions

-export([
    start_link/0,
    flush_all/0,
    create/2,
    delete/1,
    exists/1,
    user/1,
    realm/1,
    timestamp/1,
    touch/1,
    params/1,
    clear_params/1,
    get/2,
    set/3,
    forget/2,
    is_root/1,
    admin_roles/1,
    set_admin_roles/2,
    user_roles/1,
    user_roles/2,
    set_user_roles/3,
    has_admin_roles/2,
    has_user_roles/2,
    has_user_roles/3
]).

%% gen_server callbacks

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    sessions :: dict:dict(),
    root_users = []
}).

-record(session, {
    username :: binary() | string(),
    realm :: atom(),
    timestamp :: non_neg_integer(),
    is_root = false :: boolean(),
    data = dict:new(),
    admin_roles = [],
    user_roles = []
}).

%% API

-spec start_link() -> Result when
    Result :: {'ok', Pid} | 'ignore' | {'error', Error},
    Pid :: pid(),
    Error :: {'already_started', Pid} | term().

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec flush_all() -> 'ok'.

flush_all() ->
    gen_server:cast(?SERVER, flush_all).

-spec create(Username :: binary() | string(), Realm :: atom()) -> SessionId when
    SessionId :: binary().

create(Username, Realm) ->
    gen_server:call(?SERVER, {create, Username, Realm}).

-spec delete(SessionId :: binary()) -> 'ok' | {'error', Reason :: term()}.

delete(SessionId) ->
    gen_server:call(?SERVER, {delete, SessionId}).

-spec exists(SessionId :: binary()) -> boolean().

exists(SessionId) ->
    gen_server:call(?SERVER, {exists, SessionId}).

-spec user(SessionId :: binary()) -> Result when
    Result :: {'ok', Username :: binary()} | {'error', Reason :: term()}.

user(SessionId) ->
    gen_server:call(?SERVER, {user, SessionId}).

-spec realm(SessionId :: binary()) -> Result when
    Result :: {'ok', Realm :: atom()} | {'error', Reason :: term()}.

realm(SessionId) ->
    gen_server:call(?SERVER, {realm, SessionId}).

-spec timestamp(SessionId :: binary()) -> Result when
    Result :: {'ok', Timestamp :: non_neg_integer()} | {'error', Reason :: term()}.

timestamp(SessionId) ->
    gen_server:call(?SERVER, {timestamp, SessionId}).

-spec touch(SessionId :: binary()) -> 'ok' | {'error', Reason :: term()}.

touch(SessionId) ->
    gen_server:call(?SERVER, {touch, SessionId}).

-spec params(SessionId :: binary()) -> Result when
    Result :: {'ok', Params :: dict:dict()} | {'error', Reason :: term()}.

params(SessionId) ->
    gen_server:call(?SERVER, {params, SessionId}).

-spec clear_params(SessionId :: binary()) -> 'ok' | {'error', Reason :: term()}.

clear_params(SessionId) ->
    gen_server:call(?SERVER, {clear_params, SessionId}).

-spec get(SessionId :: binary(), Param :: term()) -> Result when
    Result :: {'ok', Value :: term()} | {'error', Reason :: term()}.

get(SessionId, Param) ->
    gen_server:call(?SERVER, {get, SessionId, Param}).

-spec set(SessionId :: binary(), Param :: term(), Value :: term()) -> 'ok' | {'error', Reason :: term()}.

set(SessionId, Param, Value) ->
    gen_server:call(?SERVER, {set, SessionId, Param, Value}).

-spec forget(SessionId :: binary(), Param :: term()) -> 'ok' | {'error', Reason :: term()}.

forget(SessionId, Param) ->
    gen_server:call(?SERVER, {forget, SessionId, Param}).

-spec is_root(SessionId :: binary()) -> {'ok', boolean()} | {'error', Reason :: term()}.

is_root(SessionId) ->
    gen_server:call(?SERVER, {is_root, SessionId}).

-spec admin_roles(SessionId :: binary()) -> {'ok', Roles :: [atom()]} | {'error', Reason :: term()}.

admin_roles(SessionId) ->
    gen_server:call(?SERVER, {admin_roles, SessionId}).

-spec set_admin_roles(SessionId :: binary(), Roles :: [atom()]) -> 'ok' | {'error', Reason :: term()}.

set_admin_roles(SessionId, Roles) ->
    gen_server:call(?SERVER, {set_admin_roles, SessionId, Roles}).

-spec user_roles(SessionId :: binary()) -> {'ok', Roles :: [tuple()]} | {'error', Reason :: term()}.

user_roles(SessionId) ->
    gen_server:call(?SERVER, {user_roles, SessionId}).

-spec user_roles(SessionId :: binary(), InstanceId :: binary()) -> {'ok', Roles :: [atom()]} | {'error', Reason :: term()}.

user_roles(SessionId, InstanceId) ->
    gen_server:call(?SERVER, {user_roles, SessionId, InstanceId}).

-spec set_user_roles(SessionId :: binary(), InstanceId :: binary, Roles :: [atom()]) -> 'ok' | {'error', Reason :: term()}.

set_user_roles(SessionId, InstanceId, Roles) ->
    gen_server:call(?SERVER, {set_user_roles, SessionId, InstanceId, Roles}).

-spec has_admin_roles(SessionId :: binary(), Roles :: [atom()]) -> {'ok', boolean()} | {'error', Reason :: term()}.

has_admin_roles(SessionId, Roles) ->
    gen_server:call(?SERVER, {has_admin_roles, SessionId, Roles}).

-spec has_user_roles(SessionId :: binary(), InstanceId :: binary()) -> {'ok', boolean()} | {'error', Reason :: term()}.

has_user_roles(SessionId, InstanceId) ->
    gen_server:call(?SERVER, {has_user_roles, SessionId, InstanceId}).

-spec has_user_roles(SessionId :: binary(), InstanceId :: binary(), Roles :: list(atom())) -> {'ok', boolean()} | {'error', Reason :: term()}.

has_user_roles(SessionId, InstanceId, Roles) ->
    gen_server:call(?SERVER, {has_user_roles, SessionId, InstanceId, Roles}).

%% gen_server callbacks

-spec init(Args) -> Result when
    Args :: term(),
    Result :: {'ok', State} | {'ok', State, timeout() | 'hibernate'} | {'stop', Reason} | 'ignore',
    State :: term(),
    Reason :: term().

init(_Args) ->
    erlang:send_after(?SESSION_REFRESH, self(), refresh),
    {ok, #state{sessions = dict:new()}}.

-spec handle_call(Request, From, State) -> Result when
    Request :: term(),
    From :: {pid(), Tag},
    State :: term(),
    Tag :: term(),
    Result :: {'reply', Reply, NewState}
        | {'reply', Reply, NewState, timeout() | 'hibernate'}
        | {'noreply', NewState}
        | {'noreply', NewState, timeout() | 'hibernate'}
        | {'stop', Reason, Reply, NewState}
        | {'stop', Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Reason :: term().

handle_call({create, Username, Realm}, _From, State) ->
    SessionId = gen_id(),
    IsRoot = lists:member({Username, Realm}, State#state.root_users),
    User = case zeus_db:read_user(Username, Realm) of {ok, Value} -> Value; _ -> #zeus_user{} end,
    {ok, UserRoles} = zeus_db:read_roles_user(Username),
    UserRoles1 = [{Role#zeus_role.instance_id, Role#zeus_role.roles} || Role <- UserRoles],
    {ok, RealmObjs} = zeus_db:read_realms_all(Realm),
    RealmRoles = [{RealmObj#zeus_realm.instance_id, RealmObj#zeus_realm.roles} || RealmObj <- RealmObjs],
    Roles = merge_roles(UserRoles1, RealmRoles),
    Session = #session{
        username = zeus_util:to_binary(Username),
        realm = Realm,
        timestamp = get_timestamp(),
        is_root = IsRoot,
        admin_roles = User#zeus_user.admin_roles,
        user_roles = Roles
    },
    {reply, SessionId, State#state{sessions = dict:store(SessionId, Session, State#state.sessions)}};

handle_call({delete, SessionId}, _From, State) ->
    {Reply, State1} = case dict:find(SessionId, State#state.sessions) of
        {ok, _} -> {ok, State#state{sessions = dict:erase(SessionId, State#state.sessions)}};
        error -> {{error, invalid_session}, State}
    end,
    {reply, Reply, State1};

handle_call({exists, SessionId}, _From, State) ->
    Reply = dict:is_key(SessionId, State#state.sessions),
    {reply, Reply, State};

handle_call({user, SessionId}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} -> {ok, Session#session.username};
        error -> {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call({realm, SessionId}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} -> {ok, Session#session.realm};
        error -> {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call({timestamp, SessionId}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} -> {ok, Session#session.timestamp};
        error -> {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call({touch, SessionId}, _From, State) ->
    {Reply, State1} = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            NewSessions = dict:store(SessionId, Session#session{timestamp = get_timestamp()}, State#state.sessions),
            {ok, State#state{sessions = NewSessions}};
        error ->
            {{error, invalid_session}, State}
    end,
    {reply, Reply, State1};

handle_call({params, SessionId}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} -> {ok, Session#session.data};
        error -> {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call({clear_params, SessionId}, _From, State) ->
    {Reply, State1} = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            NewSessions = dict:store(SessionId, Session#session{data = dict:new()}, State#state.sessions),
            {ok, State#state{sessions = NewSessions}};
        error ->
            {{error, invalid_session}, State}
    end,
    {reply, Reply, State1};

handle_call({get, SessionId, Param}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            case dict:find(Param, Session#session.data) of
                {ok, Value} -> {ok, Value};
                error -> {error, invalid_param}
            end;
        error ->
            {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call({set, SessionId, Param, Value}, _From, State) ->
    {Reply, State1} = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            NewData = dict:store(Param, Value, Session#session.data),
            NewSessions = dict:store(SessionId, Session#session{data = NewData}, State#state.sessions),
            {ok, State#state{sessions = NewSessions}};
        error ->
            {{error, invalid_session}, State}
    end,
    {reply, Reply, State1};

handle_call({forget, SessionId, Param}, _From, State) ->
    {Reply, State1} = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            NewData = dict:erase(Param, Session#session.data),
            NewSessions = dict:store(SessionId, Session#session{data = NewData}, State#state.sessions),
            {ok, State#state{sessions = NewSessions}};
        error ->
            Result = {error, invalid_session},
            {Result, State}
    end,
    {reply, Reply, State1};

handle_call({is_root, SessionId}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} -> {ok, Session#session.is_root};
        error -> {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call({admin_roles, SessionId}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} -> {ok, Session#session.admin_roles};
        error -> {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call({set_admin_roles, SessionId, Roles}, _From, State) ->
    {Reply, State1} = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            NewSessions = dict:store(SessionId, Session#session{admin_roles = Roles}, State#state.sessions),
            {ok, State#state{sessions = NewSessions}};
        error ->
            Result = {error, invalid_session},
            {Result, State}
    end,
    {reply, Reply, State1};

handle_call({user_roles, SessionId}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} -> {ok, Session#session.user_roles};
        error -> {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call({user_roles, SessionId, InstanceId}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} -> {ok, proplists:get_value(InstanceId, Session#session.user_roles, [])};
        error -> {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call({set_user_roles, SessionId, InstanceId, Roles}, _From, State) ->
    {Reply, State1} = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            UserRoles = lists:keyreplace(InstanceId, 1, Session#session.user_roles, {InstanceId, Roles}),
            NewSessions = dict:store(SessionId, Session#session{user_roles = UserRoles}, State#state.sessions),
            {ok, State#state{sessions = NewSessions}};
        error ->
            Result = {error, invalid_session},
            {Result, State}
    end,
    {reply, Reply, State1};

handle_call({has_admin_roles, SessionId, Roles}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} -> {ok, has_roles(Roles, Session#session.admin_roles)};
        error -> {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call({has_user_roles, SessionId, InstanceId}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            UserRoles = proplists:get_value(InstanceId, Session#session.user_roles, []),
            {ok, UserRoles =/= []};
        error ->
            {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call({has_user_roles, SessionId, InstanceId, Roles}, _From, State) ->
    Reply = case dict:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            UserRoles = proplists:get_value(InstanceId, Session#session.user_roles, []),
            {ok, has_roles(Roles, UserRoles)};
        error ->
            {error, invalid_session}
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Request, State) -> Result when
    Request :: term(),
    State :: term(),
    Result :: {'noreply', NewState} | {'noreply', NewState, timeout() | 'hibernate'} | {'stop', Reason, NewState},
    NewState :: term(),
    Reason :: term().

handle_cast(flush_all, State) ->
    {noreply, State#state{sessions = dict:new()}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> Result when
    Info :: 'timeout' | term(),
    State :: term(),
    Result :: {'noreply', NewState} | {'noreply', NewState, timeout() | 'hibernate'} | {'stop', Reason, NewState},
    NewState :: term(),
    Reason :: 'normal' | term().

handle_info(refresh, State) ->
    Timestamp = get_timestamp(),
    FunCheck = fun(_, Value) -> Timestamp - Value#session.timestamp < ?SESSION_LIFETIME end,
    Sessions = dict:filter(FunCheck, State#state.sessions),
    erlang:send_after(?SESSION_REFRESH, self(), refresh),
    {noreply, State#state{sessions = Sessions, root_users = root_users()}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason, State) -> any() when
    Reason :: 'normal' | 'shutdown' | {'shutdown', term()} | term(),
    State :: term().

terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn, State, Extra) -> Result when
    OldVsn :: Vsn | {'down', Vsn},
    State :: term(),
    Extra :: term(),
    Vsn :: term(),
    Result :: {'ok', NewState} | {'error', Reason},
    NewState :: term(),
    Reason :: term().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

gen_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    Str = io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b", [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
    list_to_binary(Str).

get_timestamp() ->
    {Mega, Secs, _} = erlang:timestamp(),
    Mega*1000000 + Secs.

root_users() ->
    case application:get_env(?WEB_APP, root) of
        undefined -> [];
        {ok, Entries} -> [{zeus_util:to_binary(Login), Realm} || {Realm, Login} <- Entries]
    end.

has_roles([], _) -> true;
has_roles(_, []) -> false;
has_roles(Required, Existing) when is_list(Required) andalso is_list(Existing) ->
    Fun = fun(Role) -> lists:member(Role, Existing) end,
    lists:all(Fun, Required).

merge_roles(UserRoles, RealmRoles) ->
    UserInstanceIds = proplists:get_keys(UserRoles),
    RealmInstanceIds = proplists:get_keys(RealmRoles),
    InstanceIds = lists:umerge(lists:usort(UserInstanceIds), lists:usort(RealmInstanceIds)),
    [begin
        URoles = proplists:get_value(InstanceId, UserRoles),
        RRoles = proplists:get_value(InstanceId, RealmRoles),
        if
            URoles =/= undefined -> {InstanceId, URoles};
            RRoles =/= undefined -> {InstanceId, RRoles};
            true -> {InstanceId, []}
        end
    end || InstanceId <- InstanceIds].
