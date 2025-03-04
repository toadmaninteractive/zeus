%% @author: PMY Archon
%% @date: 10.11.2014

-module(zeus_db).
-behaviour(gen_server).

%%
%% Include files
%%
-include_lib("stdlib/include/ms_transform.hrl").
-include("settings.hrl").
-include("zeus_shared.hrl").
-include("zeus_server.hrl").

%% API
-export([
    date_to_timestamp/2,
    start_link/0,

    % User admin roles
    create_user/1,
    read_users/0,
    read_user/2,
    update_user/1,
    delete_user/2,

    % Realm instance roles
    create_realm/1,
    read_realms/1,
    read_realms_all/1,
    read_realm/2,
    update_realm/1,
    delete_realm/2,

    % Instances
    create_instance/1,
    read_instances/0,
    read_instance/1,
    update_instance/1,
    delete_instance/1,

    % User instance roles
    create_role/1,
    read_roles/1,
    read_roles_user/1,
    read_role/3,
    update_role/1,
    delete_role/3,

    % Instance logs
    create_log/1,
    read_logs/3,
    delete_logs/1
]).

%% Behavioural callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%
%% Macros
%%
-define(SERVER, ?MODULE).

%%
%% Records
%%
-record(state, {}).

%%
%% API
%%
-spec start_link() -> Result when
    Result :: {'ok', Pid} | 'ignore' | {'error', Error},
    Pid :: pid(),
    Error :: {'already_started', Pid} | term().

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec create_user(User :: zeus_server_protocol:zeus_user()) ->
    {'ok', zeus_server_protocol:zeus_user()} | {'error', Reason :: atom()}.

create_user(User) ->
    gen_server:call(?SERVER, {create_user, User}).

-spec read_users() ->
    {'ok', list(zeus_server_protocol:zeus_user())} | {'error', Reason :: atom()}.

read_users() ->
    gen_server:call(?SERVER, read_users).

-spec read_user(Login :: binary(), Realm :: atom()) ->
    {'ok', zeus_server_protocol:zeus_user()} | {'error', Reason :: atom()}.

read_user(Login, Realm) ->
    gen_server:call(?SERVER, {read_user, Login, Realm}).

-spec update_user(User :: zeus_server_protocol:zeus_user()) ->
    {'ok', zeus_server_protocol:zeus_user()} | {'error', Reason :: atom()}.

update_user(User) ->
    gen_server:call(?SERVER, {update_user, User}).

-spec delete_user(Login :: binary(), Realm :: atom()) ->
    {'ok', Result :: boolean()} | {'error', Reason :: atom()}.

delete_user(Login, Realm) ->
    gen_server:call(?SERVER, {delete_user, Login, Realm}).

-spec create_realm(RealmObj :: zeus_server_protocol:zeus_realm()) ->
    {'ok', zeus_server_protocol:zeus_realm()} | {'error', Reason :: atom()}.

create_realm(RealmObj) ->
    gen_server:call(?SERVER, {create_realm, RealmObj}).

-spec read_realms(InstanceId :: binary()) ->
    {'ok', list(zeus_server_protocol:zeus_realm())} | {'error', Reason :: atom()}.

read_realms(InstanceId) ->
    gen_server:call(?SERVER, {read_realms, InstanceId}).

-spec read_realms_all(Realm :: atom()) ->
    {'ok', list(zeus_server_protocol:zeus_realm())} | {'error', Reason :: atom()}.

read_realms_all(Realm) ->
    gen_server:call(?SERVER, {read_realms_all, Realm}).

-spec read_realm(Realm :: atom(), InstanceId :: binary()) ->
    {'ok', zeus_server_protocol:zeus_realm()} | {'error', Reason :: atom()}.

read_realm(Realm, InstanceId) ->
    gen_server:call(?SERVER, {read_realm, Realm, InstanceId}).

-spec update_realm(RealmObj :: zeus_server_protocol:zeus_realm()) ->
    {'ok', zeus_server_protocol:zeus_realm()} | {'error', Reason :: atom()}.

update_realm(RealmObj) ->
    gen_server:call(?SERVER, {update_realm, RealmObj}).

-spec delete_realm(Realm :: atom(), InstanceId :: binary()) ->
    {'ok', Result :: boolean()} | {'error', Reason :: atom()}.

delete_realm(Realm, InstanceId) ->
    gen_server:call(?SERVER, {delete_realm, Realm, InstanceId}).

-spec create_instance(Instance :: zeus_server_protocol:zeus_instance()) ->
    {'ok', zeus_server_protocol:zeus_instance()} | {'error', Reason :: atom()}.

create_instance(Instance) ->
    gen_server:call(?SERVER, {create_instance, Instance}).

-spec read_instances() ->
    {'ok', list(zeus_server_protocol:zeus_instance())} | {'error', Reason :: atom()}.

read_instances() ->
    gen_server:call(?SERVER, read_instances).

-spec read_instance(InstanceId :: binary()) ->
    {'ok', zeus_server_protocol:zeus_instance()} | {'error', Reason :: atom()}.

read_instance(InstanceId) ->
    gen_server:call(?SERVER, {read_instance, InstanceId}).

-spec update_instance(Instance :: zeus_server_protocol:zeus_instance()) ->
    {'ok', zeus_server_protocol:zeus_instance()} | {'error', Reason :: atom()}.

update_instance(Instance) ->
    gen_server:call(?SERVER, {update_instance, Instance}).

-spec delete_instance(InstanceId :: binary()) ->
    {'ok', Result :: boolean()} | {'error', Reason :: atom()}.

delete_instance(InstanceId) ->
    gen_server:call(?SERVER, {delete_instance, InstanceId}).

-spec create_role(Role :: zeus_server_protocol:zeus_role()) ->
    {'ok', zeus_server_protocol:zeus_role()} | {'error', Reason :: atom()}.

create_role(Role) ->
    gen_server:call(?SERVER, {create_role, Role}).

-spec read_roles(InstanceId :: binary()) ->
    {'ok', list(zeus_server_protocol:zeus_role())} | {'error', Reason :: atom()}.

read_roles(InstanceId) ->
    gen_server:call(?SERVER, {read_roles, InstanceId}).

-spec read_roles_user(Login :: binary()) ->
    {'ok', list(zeus_server_protocol:zeus_role())} | {'error', Reason :: atom()}.

read_roles_user(Login) ->
    gen_server:call(?SERVER, {read_roles_user, Login}).

-spec read_role(Login :: binary(), Realm :: atom(), InstanceId :: binary()) ->
    {'ok', zeus_server_protocol:zeus_role()} | {'error', Reason :: atom()}.

read_role(Login, Realm, InstanceId) ->
    gen_server:call(?SERVER, {read_role, Login, Realm, InstanceId}).

-spec update_role(Role :: zeus_server_protocol:zeus_role()) ->
    {'ok', zeus_server_protocol:zeus_role()} | {'error', Reason :: atom()}.

update_role(Role) ->
    gen_server:call(?SERVER, {update_role, Role}).

-spec delete_role(Login :: binary(), Realm :: atom(), InstanceId :: binary()) ->
    {'ok', Result :: boolean()} | {'error', Reason :: atom()}.

delete_role(Login, Realm, InstanceId) ->
    gen_server:call(?SERVER, {delete_role, Login, Realm, InstanceId}).

-spec create_log(Log :: zeus_log()) ->
    {'ok', zeus_log()} | {'error', Reason :: atom()}.

create_log(Log) ->
    gen_server:call(?SERVER, {create_log, Log}).

-spec read_logs(InstanceId :: binary(), From :: binary() | 'undefined', To :: binary() | 'undefined') ->
    {'ok', list(zeus_log())} | {'error', Reason :: atom()}.

read_logs(InstanceId, From, To) ->
    gen_server:call(?SERVER, {read_logs, InstanceId, From, To}).

-spec delete_logs(InstanceId :: binary()) ->
    {'ok', Result :: boolean()} | {'error', Reason :: atom()}.

delete_logs(InstanceId) ->
    gen_server:call(?SERVER, {delete_logs, InstanceId}).

%%
%% Behavioural callbacks
%%
-spec init(Args) -> Result when
    Args :: term(),
    Result :: {'ok', State} | {'ok', State, timeout() | 'hibernate'} | {'stop', Reason} | 'ignore',
    State :: term(),
    Reason :: term().

init(_Args) ->
    erlang:send_after(?SESSION_REFRESH, self(), start),
    {ok, #state{}}.

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

handle_call({create_user, User}, _From, State) ->
    Reply = db_create_user(User),
    {reply, Reply, State};

handle_call(read_users, _From, State) ->
    Reply = db_read_users(),
    {reply, Reply, State};

handle_call({read_user, Login, Realm}, _From, State) ->
    Reply = db_read_user(Login, Realm),
    {reply, Reply, State};

handle_call({update_user, User}, _From, State) ->
    Reply = db_update_user(User),
    {reply, Reply, State};

handle_call({delete_user, Login, Realm}, _From, State) ->
    Reply = db_delete_user(Login, Realm),
    {reply, Reply, State};

handle_call({create_realm, RealmObj}, _From, State) ->
    Reply = db_create_realm(RealmObj),
    {reply, Reply, State};

handle_call({read_realms, InstanceId}, _From, State) ->
    Reply = db_read_realms(InstanceId),
    {reply, Reply, State};

handle_call({read_realms_all, Realm}, _From, State) ->
    Reply = db_read_realms_all(Realm),
    {reply, Reply, State};

handle_call({read_realm, Realm, InstanceId}, _From, State) ->
    Reply = db_read_realm(Realm, InstanceId),
    {reply, Reply, State};

handle_call({update_realm, RealmObj}, _From, State) ->
    Reply = db_update_realm(RealmObj),
    {reply, Reply, State};

handle_call({delete_realm, Realm, InstanceId}, _From, State) ->
    Reply = db_delete_realm(Realm, InstanceId),
    {reply, Reply, State};

handle_call({create_instance, Instance}, _From, State) ->
    Reply = db_create_instance(Instance),
    {reply, Reply, State};

handle_call(read_instances, _From, State) ->
    Reply = db_read_instances(),
    {reply, Reply, State};

handle_call({read_instance, InstanceId}, _From, State) ->
    Reply = db_read_instance(InstanceId),
    {reply, Reply, State};

handle_call({update_instance, Instance}, _From, State) ->
    Reply = db_update_instance(Instance),
    {reply, Reply, State};

handle_call({delete_instance, InstanceId}, _From, State) ->
    Reply = db_delete_instance(InstanceId),
    {reply, Reply, State};

handle_call({create_role, Role}, _From, State) ->
    Reply = db_create_role(Role),
    {reply, Reply, State};

handle_call({read_roles, InstanceId}, _From, State) ->
    Reply = db_read_roles(InstanceId),
    {reply, Reply, State};

handle_call({read_roles_user, Login}, _From, State) ->
    Reply = db_read_roles_user(Login),
    {reply, Reply, State};

handle_call({read_role, Login, Realm, InstanceId}, _From, State) ->
    Reply = db_read_role(Login, Realm, InstanceId),
    {reply, Reply, State};

handle_call({update_role, Role}, _From, State) ->
    Reply = db_update_role(Role),
    {reply, Reply, State};

handle_call({delete_role, Login, Realm, InstanceId}, _From, State) ->
    Reply = db_delete_role(Login, Realm, InstanceId),
    {reply, Reply, State};

handle_call({create_log, Log}, _From, State) ->
    Reply = db_create_log(Log),
    {reply, Reply, State};

handle_call({read_logs, InstanceId, From, To}, _From, State) ->
    Reply = db_read_logs(InstanceId, From, To),
    {reply, Reply, State};

handle_call({delete_logs, InstanceId}, _From, State) ->
    Reply = db_delete_logs(InstanceId),
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

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> Result when
    Info :: 'timeout' | term(),
    State :: term(),
    Result :: {'noreply', NewState} | {'noreply', NewState, timeout() | 'hibernate'} | {'stop', Reason, NewState},
    NewState :: term(),
    Reason :: 'normal' | term().

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason, State) -> any() when
    Reason :: 'normal' | 'shutdown' | {'shutdown', term()} | term(),
    State :: term().

terminate(_Reason, _State) ->
    dets:close(?MODULE).

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

%%
%% Internal functions
%%
db_create_user(User) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_USERS, #zeus_user{login = User#zeus_user.login, realm = User#zeus_user.realm, _ = '_'}, write) of
            [] ->
                case realm_exists(User#zeus_user.realm) of
                    true ->
                        Id = zeus_util:to_binary(zeus_util:timestamp()),
                        User1 = User#zeus_user{id = Id},
                        mnesia:write(?ZEUS_USERS, User1, write),
                        {ok, User1};
                    false ->
                        {error, invalid_realm}
                end;
            [_] ->
                {error, already_exists}
        end
    end).

db_read_users() ->
    db_run(fun() ->
        {ok, mnesia:match_object(?ZEUS_USERS, #zeus_user{_ = '_'}, read)}
    end).

db_read_user(Login, Realm) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_USERS, #zeus_user{login = Login, realm = Realm, _ = '_'}, read) of
            [] -> {error, not_exists};
            [User|_] -> {ok, User}
        end
    end).

db_update_user(User) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_USERS, #zeus_user{login = User#zeus_user.login, realm = User#zeus_user.realm, _ = '_'}, write) of
            [User1] ->
                case realm_exists(User1#zeus_user.realm) of
                    true ->
                        mnesia:write(?ZEUS_USERS, User1#zeus_user{admin_roles = User#zeus_user.admin_roles}, write),
                        {ok, User1};
                    false ->
                        {error, invalid_realm}
                end;
            [] ->
                {error, not_exists}
        end
    end).

db_delete_user(Login, Realm) ->
    db_run(fun() ->
        Users = mnesia:match_object(?ZEUS_USERS, #zeus_user{login = Login, realm = Realm, _ = '_'}, write),
        [mnesia:delete(?ZEUS_USERS, Id, write) || #zeus_user{id = Id} <- Users],
        {ok, true}
    end).

db_create_realm(RealmObj) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_REALMS, #zeus_realm{name = RealmObj#zeus_realm.name, instance_id = RealmObj#zeus_realm.instance_id, _ = '_'}, write) of
            [] ->
                case realm_exists(RealmObj#zeus_realm.name) of
                    true ->
                        Id = zeus_util:to_binary(zeus_util:timestamp()),
                        RealmObj1 = RealmObj#zeus_realm{id = Id},
                        mnesia:write(?ZEUS_REALMS, RealmObj1, write),
                        {ok, RealmObj1};
                    false ->
                        {error, invalid_realm}
                end;
            [_] ->
                {error, already_exists}
        end
    end).

db_read_realms(InstanceId) ->
    db_run(fun() ->
        {ok, mnesia:match_object(?ZEUS_REALMS, #zeus_realm{instance_id = InstanceId, _ = '_'}, read)}
    end).

db_read_realms_all(Realm) ->
    db_run(fun() ->
        {ok, mnesia:match_object(?ZEUS_REALMS, #zeus_realm{name = Realm, _ = '_'}, read)}
    end).

db_read_realm(Realm, InstanceId) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_REALMS, #zeus_realm{name = Realm, instance_id = InstanceId, _ = '_'}, read) of
            [] -> {error, not_exists};
            [Obj|_] -> {ok, Obj}
        end
    end).

db_update_realm(RealmObj) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_REALMS, #zeus_realm{name = RealmObj#zeus_realm.name, instance_id = RealmObj#zeus_realm.instance_id, _ = '_'}, write) of
            [RealmObj1] ->
                case realm_exists(RealmObj1#zeus_realm.name) of
                    true ->
                        mnesia:write(?ZEUS_REALMS, RealmObj1#zeus_realm{roles = RealmObj#zeus_realm.roles}, write),
                        {ok, RealmObj1};
                    false ->
                        {error, invalid_realm}
                end;
            [] ->
                {error, not_exists}
        end
    end).

db_delete_realm(Realm, InstanceId) ->
    db_run(fun() ->
        Objs = mnesia:match_object(?ZEUS_REALMS, #zeus_realm{name = Realm, instance_id = InstanceId, _ = '_'}, write),
        [mnesia:delete(?ZEUS_REALMS, Id, write) || #zeus_realm{id = Id} <- Objs],
        {ok, true}
    end).

db_create_instance(Instance) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_INSTANCES, #zeus_instance{key = Instance#zeus_instance.key, _ = '_'}, write) of
            [] ->
                Id = zeus_util:to_binary(zeus_util:timestamp()),
                Instance1 = Instance#zeus_instance{id = Id},
                mnesia:write(?ZEUS_INSTANCES, Instance1, write),
                {ok, Instance1};
            [_] ->
                {error, already_exists}
        end
    end).

db_read_instances() ->
    db_run(fun() ->
        {ok, mnesia:match_object(?ZEUS_INSTANCES, #zeus_instance{_ = '_'}, read)}
    end).

db_read_instance(InstanceId) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_INSTANCES, #zeus_instance{id = InstanceId, _ = '_'}, read) of
            [] -> {error, not_exists};
            [Instance|_] -> {ok, Instance}
        end
    end).

db_update_instance(Instance) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_INSTANCES, #zeus_instance{id = Instance#zeus_instance.id, _ = '_'}, write) of
            [Instance1] ->
                Instance2 = Instance1#zeus_instance{
                    key = Instance#zeus_instance.key,
                    title = Instance#zeus_instance.title,
                    api_root = Instance#zeus_instance.api_root,
                    api_key = Instance#zeus_instance.api_key
                },
                mnesia:write(?ZEUS_INSTANCES, Instance2, write),
                {ok, Instance2};
            [] ->
                {error, not_exists}
        end
    end).

db_delete_instance(InstanceId) ->
    db_run(fun() ->
        Instances = mnesia:match_object(?ZEUS_INSTANCES, #zeus_instance{id = InstanceId, _ = '_'}, write),
        [mnesia:delete(?ZEUS_INSTANCES, Id, write) || #zeus_instance{id = Id} <- Instances],
        {ok, true}
    end).

db_create_role(Role) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_ROLES, #zeus_role{login = Role#zeus_role.login, realm = Role#zeus_role.realm, instance_id = Role#zeus_role.instance_id, _ = '_'}, write) of
            [] ->
                case realm_exists(Role#zeus_role.realm) of
                    true ->
                        Id = zeus_util:to_binary(zeus_util:timestamp()),
                        Role1 = Role#zeus_role{id = Id},
                        mnesia:write(?ZEUS_ROLES, Role1, write),
                        {ok, Role1};
                    false ->
                        {error, invalid_realm}
                end;
            [_] ->
                {error, already_exists}
        end
    end).

db_read_roles(InstanceId) ->
    db_run(fun() ->
        {ok, mnesia:match_object(?ZEUS_ROLES, #zeus_role{instance_id = InstanceId, _ = '_'}, read)}
    end).

db_read_roles_user(Login) ->
    db_run(fun() ->
        {ok, mnesia:match_object(?ZEUS_ROLES, #zeus_role{login = Login, _ = '_'}, read)}
    end).

db_read_role(Login, Realm, InstanceId) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_ROLES, #zeus_role{login = Login, realm = Realm, instance_id = InstanceId, _ = '_'}, read) of
            [] -> {error, not_exists};
            [Role|_] -> {ok, Role}
        end
    end).

db_update_role(Role) ->
    db_run(fun() ->
        case mnesia:match_object(?ZEUS_ROLES, #zeus_role{login = Role#zeus_role.login, realm = Role#zeus_role.realm, instance_id = Role#zeus_role.instance_id, _ = '_'}, write) of
            [Role1] ->
                case realm_exists(Role1#zeus_role.realm) of
                    true ->
                        mnesia:write(?ZEUS_ROLES, Role1#zeus_role{roles = Role#zeus_role.roles}, write),
                        {ok, Role1};
                    false ->
                        {error, invalid_realm}
                end;
            [] ->
                {error, not_exists}
        end
    end).

db_delete_role(Login, Realm, InstanceId) ->
    db_run(fun() ->
        Roles = mnesia:match_object(?ZEUS_ROLES, #zeus_role{login = Login, realm = Realm, instance_id = InstanceId, _ = '_'}, write),
        [mnesia:delete(?ZEUS_ROLES, Id, write) || #zeus_role{id = Id} <- Roles],
        {ok, true}
    end).

db_create_log(Log) ->
    db_run(fun() ->
        Id = zeus_util:to_binary(zeus_util:timestamp()),
        Now = erlang:localtime(),
        TimeStamp = zeus_util:timestamp(Now),
        DateTime = zeus_util:iso_8601_fmt(Now),
        Log1 = Log#zeus_log{id = Id, timestamp = TimeStamp, datetime = DateTime},
        mnesia:write(?ZEUS_LOGS, Log1, write),
        {ok, Log1}
    end).

db_read_logs(InstanceId, From, To) ->
    From1 = date_to_timestamp(From, <<"00:00:00">>),
    To1 = date_to_timestamp(To, <<"23:59:59">>),
    db_run(fun() ->
            MatchSpec = ets:fun2ms(fun
                (#zeus_log{timestamp = Ts, instance_id = Id} = Item) when Id =:= InstanceId, Ts >= From1, Ts =< To1 -> Item
            end),
            {ok, mnesia:select(?ZEUS_LOGS, MatchSpec)}
    end).

db_delete_logs(InstanceId) ->
    db_run(fun() ->
        Logs = mnesia:match_object(?ZEUS_LOGS, #zeus_log{instance_id = InstanceId, _ = '_'}, write),
        [mnesia:delete(?ZEUS_LOGS, Id, write) || #zeus_log{id = Id} <- Logs],
        {ok, true}
    end).

db_run(Fun) when is_function(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, {ok, Result}} -> {ok, Result};
        {atomic, {error, Reason}} when is_atom(Reason) -> {error, Reason};
        {aborted, Reason} -> 
            error_logger:error_msg("Zeus database error ~p", [Reason]),
            {error, failed}
    end.

realm_exists(Realm) ->
    {ok, Realms} = application:get_env(zeus, authentication_realms),
    lists:member(Realm, Realms).

date_to_timestamp(undefined, Time) ->
    date_to_timestamp(<<"1970-01-01">>, Time);
date_to_timestamp(Date, undefined) ->
    date_to_timestamp(Date, <<"00:00:00">>);
date_to_timestamp(<<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary>>, <<Hour:2/binary, ":", Min:2/binary, ":", Sec:2/binary>>) ->
    DateTime = {
        {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
        {binary_to_integer(Hour), binary_to_integer(Min), binary_to_integer(Sec)}
    },
    zeus_util:timestamp(DateTime);
date_to_timestamp(_, _) ->
    date_to_timestamp(<<"1970-01-01">>, <<"00:00:00">>).