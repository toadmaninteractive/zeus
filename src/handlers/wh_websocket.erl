-module(wh_websocket).

%% Include files

-include("zeus_server.hrl").
-include("settings.hrl").

%% Exported functions

-export([
    init/2,
    websocket_handle/3,
    websocket_info/3
]).

%% API

init(Req, Opts) ->
    erlang:send_after(0, self(), init),
    {cowboy_websocket, Req, Opts}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%% Initialize websocket, authorize user

websocket_info(init, Req, State) ->
    % Get session ID from cookies
    Cookies = cowboy_req:parse_cookies(Req),
    SessionId = proplists:get_value(?SESSION_ID_VAR, Cookies, undefined),

    %% Grab instance ID and path parts, construct valid path
    InstanceId = cowboy_req:binding(instance_id, Req),
    PathParts = cowboy_req:path_info(Req),
    Path = <<"/", (zeus_util:binary_join(PathParts, <<"/">>))/binary>>,

    % Check if given instance exists
    Reply = case zeus_db:read_instance(InstanceId) of
        {ok, #zeus_instance{api_root = ApiRoot, api_key = ApiKey}} ->
            % Check if user is authorized to access this instance
            case authorized(SessionId, InstanceId) of
                true ->
                    % Try connect to remote websocket
                    Opts = get_opts(SessionId, InstanceId),
                    case connect(ApiRoot, Path, ApiKey, Opts) of
                        {ok, _Pid, _Ref} ->
                            Frame = get_frame(),
                            erlang:send_after(10, self(), pick),
                            Frame;
                        {error, failed} ->
                            % Connection failed
                            Json = #{<<"result">> => false, <<"error">> => <<"connection_failed">>},
                            {close, ?ws_code_not_authorized, jsx:encode(Json)};
                        {error, timeout} ->
                            % Connection timed out
                            Json = #{<<"result">> => false, <<"error">> => <<"connection_timeout">>},
                            {close, ?ws_code_not_authorized, jsx:encode(Json)}
                    end;
                false ->
                    % Not authorized
                    Json = #{<<"result">> => false, <<"error">> => <<"not_authorized">>},
                    {close, ?ws_code_not_authorized, jsx:encode(Json)}
            end;
        {error, not_exists} ->
            Json = #{<<"result">> => false, <<"error">> => <<"invalid_instance">>},
            {close, ?ws_code_not_found, jsx:encode(Json)}
    end,

    {reply, Reply, Req, State};

websocket_info(pick, Req, State) ->
    Frame = get_frame(),
    erlang:send_after(10, self(), pick),
    {reply, Frame, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

%% Local functions

%% Check if user is authorized to access target instance

authorized(undefined, _) ->
    false;
authorized(SessionId, InstanceId) ->
    case zeus_session:exists(SessionId) of
        true ->
            {ok, IsRoot} = zeus_session:is_root(SessionId),
            {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_instances]),
            {ok, IsAuthorizedUser} = zeus_session:has_user_roles(SessionId, InstanceId),
            IsRoot or IsAdmin or IsAuthorizedUser;
        false ->
            false
    end.

get_host_port(HostStr) ->
    case binary:split(HostStr, <<":">>, [global]) of
        [Host, Port] -> {Host, binary_to_integer(binary:replace(Port, <<"/">>, <<>>, [global]))};
        [Host] -> {Host, 443}
    end.

%% Get zeus specific options

get_opts(SessionId, InstanceId) ->
    % Check if user is root or administrator
    {ok, IsRoot} = zeus_session:is_root(SessionId),
    {ok, IsAdmin} = zeus_session:has_admin_roles(SessionId, [manage_instances]),

    % Get user roles and construct zeus roles string
    {ok, UserRoles} = zeus_session:user_roles(SessionId, InstanceId),
    ZeusRoles = string:join([atom_to_list(Role) || Role <- UserRoles], ", "),

    % Get username, realm and construct zeus user
    {ok, Username} = zeus_session:user(SessionId),
    {ok, Realm} = zeus_session:realm(SessionId),
    ZeusUser = io_lib:format("~s@~s", [Username, Realm]),

    #{
        zeus_user => ZeusUser,
        zeus_administrator => atom_to_list(IsRoot or IsAdmin),
        zeus_roles => ZeusRoles
    }.

%% Connect to target instance websocket

connect(ApiRoot, Path, ApiKey, Opts) ->
    {Transport, Host, Port} = case ApiRoot of
        <<"https://", HS/binary>> ->
            % Secure
            {H, P} = get_host_port(HS),
            {ssl, H, P};
        <<"http://", HS/binary>> ->
            % Insecure
            {H, P} = get_host_port(HS),
            {tcp, H, P}
    end,

    % Prepare options
    Host1 = binary_to_list(Host),
    #{zeus_user := ZeusUser, zeus_administrator := ZeusAdministrator, zeus_roles := ZeusRoles} = Opts,

    % Prepare headers
    Headers = [
        {<<"x-zeus-user">>, ZeusUser},
        {<<"x-zeus-administrator">>, ZeusAdministrator},
        {<<"x-zeus-roles">>, ZeusRoles},
        {<<"x-zeus-api-key">>, binary_to_list(ApiKey)}
    ],

    % Connect
    {ok, Pid} = gun:open(Host1, Port, #{retry => 0, transport => Transport}),
    {ok, http} = gun:await_up(Pid),
    Ref = monitor(process, Pid),
    % gun:ws_upgrade(Pid, Path, Headers, #{compress => true}),
    gun:ws_upgrade(Pid, Path, Headers, #{}),
    receive
        {gun_ws_upgrade, Pid, ok, _} -> {ok, Pid, Ref};
        _Err -> {error, failed}
    after 10000 ->
        {error, timeout}
    end.

close(Pid, Ref) ->
    demonitor(Ref),
    gun:close(Pid),
    gun:flush(Pid).

get_frame() ->
    receive
        {gun_ws, _ConnPid, Frame} -> Frame
    end.
