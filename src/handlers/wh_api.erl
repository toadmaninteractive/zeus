-module(wh_api).

-behaviour(cowboy_rest).

%% Include files

-include_lib("kernel/include/logger.hrl").
-include("settings.hrl").
-include("zeus_shared.hrl").
-include("zeus_server.hrl").

%% Exported functions

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    charsets_provided/2,
    delete_resource/2,
    handle_json/2
]).

%% API

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    HttpMethods = [<<"GET">>, <<"PUT">>, <<"POST">>, <<"PATCH">>, <<"DELETE">>],
    {HttpMethods, Req, State}.

content_types_accepted(Req, State) ->
    ContentTypes = [
        {{<<"application">>, <<"json">>, '*'}, handle_json}
    ],
    {ContentTypes, Req, State}.

content_types_provided(Req, State) ->
    ContentTypes = [
        {{<<"application">>, <<"json">>, '*'}, handle_json}
    ],
    {ContentTypes, Req, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

delete_resource(Req, State) ->
    handle_json(Req, State).

-spec handle_json(cowboy_req:req(), term()) ->
    {iodata() | boolean() | atom(), cowboy_req:req(), term()}.

handle_json(Req, State) ->
    Method = cowboy_req:method(Req),
    <<"/_utils/api/", Path/binary>> = cowboy_req:path(Req),
    Cookies = cowboy_req:parse_cookies(Req),
    SessionId = proplists:get_value(?SESSION_ID_VAR, Cookies, undefined),
    IsAuthenticated = zeus_session:touch(SessionId) =:= ok,
    Qs = cowboy_req:qs(Req),
    QsVals = cowboy_req:parse_qs(Req),
    PathInfo = cowboy_req:path_info(Req),
    {Json, Req1} = zeus_util:json_body(Req),
    PathInfo1 = case PathInfo of
        undefined -> [];
        _ -> PathInfo
    end,
    ApiRequest = #api_req{
        method = zeus_util:http_method(Method),
        path = <<"/", Path/binary>>,
        path_parts = binary:split(Path, <<"/">>, [global]),
        pathinfo = zeus_util:binary_join(PathInfo1, <<"/">>),
        authenticated = IsAuthenticated,
        session_id = SessionId,
        qs = Qs,
        qs_vals = QsVals,
        json = Json
    },
    rest_api(ApiRequest, Req1, State).

%% Local functions

%% /auth/version
rest_api(#api_req{method = get, path = <<"/auth/version">>}, Req, State) ->
    {ok, Vsn} = application:get_key(?WEB_APP, vsn),
    Json = #{version => zeus_util:to_binary(Vsn)},
    zeus_util:reply_json(get, Json, Req, State);

%% /auth/realms
rest_api(#api_req{method = get, path = <<"/auth/realms">>}, Req, State) ->
    Json = [#{realm => zeus_util:to_binary(Realm), title => Title} || {Realm, Title} <- zeus_util:cerberus_realms()],
    zeus_util:reply_json(get, Json, Req, State);

%% /auth/login
rest_api(#api_req{method = post, path = <<"/auth/login">>, authenticated = true}, Req, State) ->
    zeus_util:reply_igor(post, #reply_status{result = false, error = <<"already_logged_in">>}, Req, State);

rest_api(#api_req{method = post, path = <<"/auth/login">>, authenticated = false, json = Obj}, Req, State) ->
    Username = maps:get(<<"username">>, Obj, <<>>),
    Password = maps:get(<<"password">>, Obj, <<>>),
    Realm = binary_to_existing_atom(maps:get(<<"realm">>, Obj, <<"undefined">>), latin1),
    case zeus_util:cerberus_authenticate(Username, Password, Realm) of
        ok ->
            SessionId = zeus_session:create(Username, Realm),
            Req1 = cowboy_req:set_resp_cookie(?SESSION_ID_VAR, SessionId, Req, #{path => <<"/">>, max_age => ?SESSION_LIFETIME}),
            {ok, IsRoot} = zeus_session:is_root(SessionId),
            {ok, AdminRoles} = zeus_session:admin_roles(SessionId),
            {ok, UserRoles} = zeus_session:user_roles(SessionId),
            Json = #{
                result => true,
                username => Username,
                realm => Realm,
                admin_roles => AdminRoles,
                user_roles => maps:from_list(UserRoles),
                is_root => IsRoot
            },
            zeus_util:reply_json(post, Json, Req1, State);
        {error, Reason} ->
            ?LOG_WARNING(#{what => zeus_auth_failed, reason => Reason, user => Username, realm => Realm}),
            ReasonStr = case Reason of
                invalidCredentials -> <<"Invalid credentials">>;
                unknown_user -> <<"Unknown user">>;
                anonymous_auth -> <<"No password given">>;
                _ -> <<"Server error">>
            end,
            zeus_util:reply_igor(post, #reply_status{result = false, error = ReasonStr}, Req, State)
    end;

%% Not authenticated...
rest_api(#api_req{method = Method, authenticated = false}, Req, State) ->
    zeus_util:reply_igor(Method, #reply_status{result = false, error = <<"not_authenticated">>}, Req, State);

%% /auth/logout
rest_api(#api_req{method = get, path = <<"/auth/logout">>, session_id = SessionId}, Req, State) ->
    case zeus_session:delete(SessionId) of
        ok -> zeus_util:reply_igor(get, #reply_status{result = true}, zeus_util:reset_session(Req), State);
        _ -> zeus_util:reply_igor(get, #reply_status{result = false, error = <<"invalid_request">>}, Req, State)
    end;

%% /auth/state
rest_api(#api_req{method = get, path = <<"/auth/state">>, session_id = SessionId}, Req, State) ->
    {ok, Username} = zeus_session:user(SessionId),
    {ok, Realm} = zeus_session:realm(SessionId),
    {ok, AdminRoles} = zeus_session:admin_roles(SessionId),
    {ok, UserRoles} = zeus_session:user_roles(SessionId),
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    Json = #{
        result => true,
        username => Username,
        realm => Realm,
        admin_roles => AdminRoles,
        user_roles => maps:from_list(UserRoles),
        is_root => IsRoot
    },
    zeus_util:reply_json(get, Json, Req, State);

%% /auth/users
rest_api(#api_req{method = post, path = <<"/auth/users">>, json = Json, session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    if
        IsRoot or IsAdmin ->
            User = zeus_server_protocol:zeus_user_from_json(Json),
            case zeus_db:create_user(User) of
                {ok, User1} -> zeus_util:reply_json(post, zeus_server_protocol:zeus_user_to_json(User1), Req, State);
                {error, Reason} -> zeus_util:reply_igor(post, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = get, path = <<"/auth/users">>, session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    if
        IsRoot or IsAdmin ->
            case zeus_db:read_users() of
                {ok, Users} -> zeus_util:reply_json(get, [zeus_server_protocol:zeus_user_to_json(User) || User <- Users], Req, State);
                {error, Reason} -> zeus_util:reply_igor(get, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = get, path_parts = [<<"auth">>, <<"users">>, Realm, Login], session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    if
        IsRoot or IsAdmin ->
            case zeus_db:read_user(Login, binary_to_existing_atom(Realm, latin1)) of
                {ok, User} -> zeus_util:reply_json(get, zeus_server_protocol:zeus_user_to_json(User), Req, State);
                {error, Reason} -> zeus_util:reply_igor(get, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = put, path_parts = [<<"auth">>, <<"users">>, Realm, Login], json = Json, session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    if
        IsRoot or IsAdmin ->
            User = zeus_server_protocol:zeus_user_from_json(Json),
            User1 = User#zeus_user{login = Login, realm = binary_to_existing_atom(Realm, latin1)},
            case zeus_db:update_user(User1) of
                {ok, User2} -> zeus_util:reply_json(put, zeus_server_protocol:zeus_user_to_json(User2), Req, State);
                {error, Reason} -> zeus_util:reply_igor(put, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = delete, path_parts = [<<"auth">>, <<"users">>, Realm, Login], session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    if
        IsRoot or IsAdmin ->
            case zeus_db:delete_user(Login, binary_to_existing_atom(Realm, latin1)) of
                {ok, true} -> zeus_util:reply_igor(delete, #reply_status{result = true}, Req, State);
                {ok, false} -> zeus_util:reply_igor(delete, #reply_status{result = false, error = <<"delete_failed">>}, Req, State);
                {error, Reason} -> zeus_util:reply_igor(delete, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

%% /auth/roles
rest_api(#api_req{method = post, path_parts = [<<"auth">>, <<"roles">>, InstanceId], json = Json, session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId, [admin]),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            Role = zeus_server_protocol:zeus_role_from_json(Json),
            Role1 = Role#zeus_role{instance_id = InstanceId},
            case zeus_db:create_role(Role1) of
                {ok, Role2} -> zeus_util:reply_json(post, zeus_server_protocol:zeus_role_to_json(Role2), Req, State);
                {error, Reason} -> zeus_util:reply_igor(post, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = get, path_parts = [<<"auth">>, <<"roles">>, InstanceId], session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId, [admin]),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            case zeus_db:read_roles(InstanceId) of
                {ok, Roles} -> zeus_util:reply_json(get, [zeus_server_protocol:zeus_role_to_json(Role) || Role <- Roles], Req, State);
                {error, Reason} -> zeus_util:reply_igor(get, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = get, path_parts = [<<"auth">>, <<"roles">>, InstanceId, Realm, Login], session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId, [admin]),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            case zeus_db:read_role(Login, binary_to_existing_atom(Realm, latin1), InstanceId) of
                {ok, Role} -> zeus_util:reply_json(get, zeus_server_protocol:zeus_role_to_json(Role), Req, State);
                {error, Reason} -> zeus_util:reply_igor(get, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = put, path_parts = [<<"auth">>, <<"roles">>, InstanceId, Realm, Login], json = Json, session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId, [admin]),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            Role = zeus_server_protocol:zeus_role_from_json(Json),
            Role1 = Role#zeus_role{login = Login, realm = binary_to_existing_atom(Realm, latin1), instance_id = InstanceId},
            case zeus_db:update_role(Role1) of
                {ok, Role2} -> zeus_util:reply_json(put, zeus_server_protocol:zeus_role_to_json(Role2), Req, State);
                {error, Reason} -> zeus_util:reply_igor(put, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = delete, path_parts = [<<"auth">>, <<"roles">>, InstanceId, Realm, Login], session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId, [admin]),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            case zeus_db:delete_role(Login, binary_to_existing_atom(Realm, latin1), InstanceId) of
                {ok, true} -> zeus_util:reply_igor(delete, #reply_status{result = true}, Req, State);
                {ok, false} -> zeus_util:reply_igor(delete, #reply_status{result = false, error = <<"delete_failed">>}, Req, State);
                {error, Reason} -> zeus_util:reply_igor(delete, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

%% /auth/realm-roles
rest_api(#api_req{method = post, path_parts = [<<"auth">>, <<"realm-roles">>, InstanceId], json = Json, session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId, [admin]),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            RealmObj = zeus_server_protocol:zeus_realm_from_json(Json),
            RealmObj1 = RealmObj#zeus_realm{instance_id = InstanceId},
            case zeus_db:create_realm(RealmObj1) of
                {ok, RealmObj2} -> zeus_util:reply_json(post, zeus_server_protocol:zeus_realm_to_json(RealmObj2), Req, State);
                {error, Reason} -> zeus_util:reply_igor(post, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = get, path_parts = [<<"auth">>, <<"realm-roles">>, InstanceId], session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId, [admin]),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            case zeus_db:read_realms(InstanceId) of
                {ok, RealmObjs} -> zeus_util:reply_json(get, [zeus_server_protocol:zeus_realm_to_json(RealmObj) || RealmObj <- RealmObjs], Req, State);
                {error, Reason} -> zeus_util:reply_igor(get, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = get, path_parts = [<<"auth">>, <<"realm-roles">>, InstanceId, Realm], session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId, [admin]),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            case zeus_db:read_realm(binary_to_existing_atom(Realm, latin1), InstanceId) of
                {ok, RealmObj} -> zeus_util:reply_json(get, zeus_server_protocol:zeus_realm_to_json(RealmObj), Req, State);
                {error, Reason} -> zeus_util:reply_igor(get, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = put, path_parts = [<<"auth">>, <<"realm-roles">>, InstanceId, Realm], json = Json, session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId, [admin]),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            RealmObj = zeus_server_protocol:zeus_realm_from_json(Json),
            RealmObj1 = RealmObj#zeus_realm{name = binary_to_existing_atom(Realm, latin1), instance_id = InstanceId},
            case zeus_db:update_realm(RealmObj1) of
                {ok, _} -> zeus_util:reply_json(put, zeus_server_protocol:zeus_realm_to_json(RealmObj1), Req, State);
                {error, Reason} -> zeus_util:reply_igor(put, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = delete, path_parts = [<<"auth">>, <<"realm-roles">>, InstanceId, Realm], session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_users]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId, [admin]),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            case zeus_db:delete_realm(binary_to_existing_atom(Realm, latin1), InstanceId) of
                {ok, true} -> zeus_util:reply_igor(delete, #reply_status{result = true}, Req, State);
                {ok, false} -> zeus_util:reply_igor(delete, #reply_status{result = false, error = <<"delete_failed">>}, Req, State);
                {error, Reason} -> zeus_util:reply_igor(delete, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

%% /instances/entries
rest_api(#api_req{method = post, path_parts = [<<"instances">>, <<"entries">>], json = Json, session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_instances]),
    if
        IsRoot or IsAdmin ->
            Instance = zeus_server_protocol:zeus_instance_from_json(Json),
            case zeus_db:create_instance(Instance) of
                {ok, Instance1} ->
                    zeus_util:reply_json(post, zeus_server_protocol:zeus_instance_to_json(Instance1), Req, State);
                {error, Reason} ->
                    zeus_util:reply_igor(post, #reply_status{result = false, error = zeus_util:error_to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = get, path_parts = [<<"instances">>, <<"entries">> | Tail], session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_instances]),
    {ok, UserRoles} = zeus_session:user_roles(SessionId),
    ReplyJson = case Tail of
        [InstanceId|_] ->
            {ok, Item} = zeus_db:read_instance(InstanceId),
            case filter_instances([Item], UserRoles, IsRoot or IsAdmin) of
                [Item1] -> zeus_server_protocol:zeus_instance_to_json(Item1);
                _ -> zeus_server_protocol:reply_status_to_json(#reply_status{result = false, error = <<"unprivileged_access">>})
            end;
        _ ->
            {ok, Items} = zeus_db:read_instances(),
            Items1 = filter_instances(Items, UserRoles, IsRoot or IsAdmin),
            [zeus_server_protocol:zeus_instance_to_json(Item) || Item <- Items1]
    end,
    zeus_util:reply_json(get, ReplyJson, Req, State);

rest_api(#api_req{method = put, path_parts = [<<"instances">>, <<"entries">>, InstanceId], json = Json, session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_instances]),
    if
        IsRoot or IsAdmin ->
            Instance = zeus_server_protocol:zeus_instance_from_json(Json),
            case zeus_db:read_instance(InstanceId) of
                {ok, _} ->
                    case zeus_db:update_instance(Instance) of
                        {ok, Instance1} -> zeus_util:reply_json(post, zeus_server_protocol:zeus_instance_to_json(Instance1), Req, State);
                        {error, Reason} -> zeus_util:reply_igor(put, #reply_status{result = false, error = zeus_util:error_to_binary(Reason)}, Req, State)
                    end;
                {error, Reason} ->
                    zeus_util:reply_igor(put, #reply_status{result = false, error = zeus_util:error_to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

rest_api(#api_req{method = delete, path_parts = [<<"instances">>, <<"entries">>, InstanceId], session_id = SessionId}, Req, State) when InstanceId =/= undefined ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_instances]),
    if
        IsRoot or IsAdmin ->
            case zeus_db:delete_instance(InstanceId) of
                {ok, Result} -> zeus_util:reply_igor(delete, #reply_status{result = Result}, Req, State);
                {error, Reason} -> zeus_util:reply_igor(delete, #reply_status{result = false, error = zeus_util:error_to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

% /instance/logs/%instance_id%
rest_api(#api_req{method = get, path_parts = [<<"instance">>, <<"logs">>, InstanceId], qs_vals = QsVals, session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_instances]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            From = proplists:get_value(<<"date_from">>, QsVals, <<"1970-01-01">>),
            To = proplists:get_value(<<"date_to">>, QsVals, date_now()),
            case zeus_db:read_logs(InstanceId, From, To) of
                {ok, Items} ->
                    Items1 = sort_logs(filter_logs(Items, QsVals)),
                    zeus_util:reply_json(get, [pack_log(Item) || Item <- Items1], Req, State);
                {error, Reason} ->
                    zeus_util:reply_igor(get, #reply_status{result = false, error = zeus_util:to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

% /instances/link
rest_api(#api_req{method = Method, path_parts = [<<"instances">>, <<"link">>, InstanceId | Tail], qs = Qs, json = Json, session_id = SessionId}, Req, State) ->
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_instances]),
    {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId),
    if
        IsRoot or IsAdmin or IsAuthorizedUser ->
            PathInfo = zeus_util:binary_join(Tail, <<"/">>),
            case zeus_db:read_instance(InstanceId) of
                {ok, Instance} ->
                    ApiRoot = Instance#zeus_instance.api_root,
                    ApiKey = Instance#zeus_instance.api_key,
                    Url = make_url(ApiRoot, PathInfo, ApiKey, Qs),
                    Content = case Json of undefined -> <<>>; _ -> zeus_util:encode(Json) end,
                    ContentType = {"Content-Type", "application/json; charset=utf-8"},
                    Headers = case Method of
                        post -> [ContentType];
                        put -> [ContentType];
                        patch -> [ContentType];
                        _ -> []
                    end,
                    {ok, UserRoles} = zeus_session:user_roles(SessionId, InstanceId),
                    ZeusRoles = string:join([atom_to_list(Role) || Role <- UserRoles], ", "),
                    {ok, Username} = zeus_session:user(SessionId),
                    {ok, Realm} = zeus_session:realm(SessionId),
                    Headers1 = [
                        {"X-Zeus-User", io_lib:format("~s@~s", [Username, Realm])},
                        {"X-Zeus-Administrator", atom_to_list(IsRoot or IsAdmin)},
                        {"X-Zeus-Roles", ZeusRoles}
                    | Headers],
                    case zeus_util:http_req(Method, Headers1, Url, Content) of
                        {ok, RStatus, RHeaders, Body} ->
                            LogData = proplists:get_value(<<"x-zeus-log">>, RHeaders),
                            RHeaders1 = proplists:delete(<<"x-zeus-log">>, RHeaders),
                            RHeaders2 = [{cowboy_bstr:to_lower(K), V} || {K, V} <- RHeaders1],
                            case LogData of
                                undefined -> ok;
                                _ -> store_log(SessionId, InstanceId, Method, Url, Qs, Json, LogData)
                            end,
                            zeus_util:reply_body_status(RStatus, maps:from_list(RHeaders2), Body, Req, State);
                        {error, Reason} ->
                            zeus_util:reply_igor(Method, #reply_status{result = false, error = zeus_util:error_to_binary(Reason)}, Req, State)
                    end;
                {error, Reason} ->
                    zeus_util:reply_igor(Method, #reply_status{result = false, error = zeus_util:error_to_binary(Reason)}, Req, State)
            end;
        true ->
            zeus_util:reply_body_status(403, #{}, <<"Forbidden">>, Req, State)
    end;

%% Catch-up rule
rest_api(#api_req{method = Method}, Req, State) ->
    zeus_util:reply_igor(Method, #reply_status{result = false, error = <<"invalid_request">>}, Req, State).

make_url(ApiRoot, PathInfo, ApiKey, Qs) ->
    Qs1 = case Qs of
        <<>> -> <<>>;
        _ -> <<"&", Qs/binary>>
    end,
    IsLastSlash = case ApiRoot of <<>> -> false; _ -> binary:last(ApiRoot) =:= $/ end,
    IsFirstSlash = case PathInfo of <<>> -> false; _ -> binary:first(PathInfo) =:= $/ end,
    Separator = if IsLastSlash or IsFirstSlash -> <<>>; true -> <<"/">> end,
    <<ApiRoot/binary, Separator/binary, PathInfo/binary, "?api_key=", ApiKey/binary, Qs1/binary>>.

filter_instances(Items, _, true) ->
    Items;
filter_instances(Items, UserRoles, _) ->
    Fun = fun(Instance, Acc) ->
        Instance1 = Instance#zeus_instance{api_root = <<>>, api_key = <<>>},
        case proplists:get_value(Instance1#zeus_instance.id, UserRoles) of
            Roles when is_list(Roles), Roles =/= [] -> [Instance1 | Acc];
            _ -> Acc
        end
    end,
    lists:foldl(Fun, [], Items).

store_log(_, _, _, _, _, _, undefined) ->
    ok;
store_log(SessionId, InstanceId, Method, Url, Qs, Body, LogData) ->
    {ok, Username} = zeus_session:user(SessionId),
    {ok, Realm} = zeus_session:realm(SessionId),
    AgentReply = zeus_util:decode(base64:decode(LogData)),
    Message = maps:get(<<"message">>, AgentReply, <<"No message">>),
    Data = maps:get(<<"data">>, AgentReply),
    Item = #zeus_log{
        instance_id = InstanceId,
        username = Username,
        realm = Realm,
        http_method = Method,
        url = Url,
        qs = Qs,
        body = Body,
        agent_data = Data,
        agent_message = Message
    },
    zeus_db:create_log(Item).

date_now() ->
    {{Year, Month, Day}, _} = erlang:localtime(),
    IoList = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Year, Month, Day]),
    iolist_to_binary(IoList).

pack_log(#zeus_log{id = Id, timestamp = Timestamp, datetime = DateTime, username = Username, realm = Realm, agent_message = Message}) ->
    #{
        id => Id,
        timestamp => Timestamp,
        datetime => DateTime,
        username => Username,
        realm => Realm,
        message => Message
    }.

filter_logs(Items, QsVals) ->
    QsVals1 = proplists:delete(<<"date_from">>, QsVals),
    QsVals2 = proplists:delete(<<"date_to">>, QsVals1),
    lists:reverse(filter_logs(Items, QsVals2, [])).

filter_logs([], _, Acc) ->
    Acc;
filter_logs([#zeus_log{agent_data = Json} = Item|Tail], QsVals, Acc) ->
    case check_obj(Json, QsVals) of
        true -> filter_logs(Tail, QsVals, [Item|Acc]);
        false -> filter_logs(Tail, QsVals, Acc)
    end.

sort_logs(Items) ->
    lists:reverse(lists:keysort(#zeus_log.timestamp, Items)).

check_obj(Json, QsVals) ->
    Fun = fun({Key, Value}) ->
        Value =:= zeus_util:to_binary(maps:get(Key, Json, <<>>))
    end,
    lists:all(Fun, QsVals).
