-module(zeus_web_init).

%% Include files

-include("settings.hrl").

%% Exported functions

-export([
    start_cowboy/0
]).

%% API

start_cowboy() ->
    % Params
    BindIp = application:get_env(?WEB_APP, bind_ip, ?WEB_ADDR),
    BindPort = application:get_env(?WEB_APP, bind_port, ?WEB_PORT),
    UseTls = application:get_env(?WEB_APP, use_tls, false),
    CertFile = application:get_env(?WEB_APP, certfile, undefined),
    KeyFile = application:get_env(?WEB_APP, keyfile, undefined),
    Production = application:get_env(?WEB_APP, production, false),

    % Paths
    PrivDir = code:priv_dir(?WEB_APP),
    StaticDir = filename:join([PrivDir, "static"]),
    ThemeDir = filename:join([PrivDir, "themes", ?ACTIVE_THEME]),
    FavIcon = "favicon.ico",

    FrontendProps = [
        {static_dir, StaticDir},
        {theme_dir, ThemeDir},
        {production, Production}
    ],

    Middlewares = [
        % zeus_web_log,
        zeus_web_hook,
        cowboy_router,
        cowboy_handler
    ],

    Dispatch = cowboy_router:compile([
        {'_', [
            % FavIcon
            {"/" ++ FavIcon, cowboy_static, {file, filename:join([StaticDir, FavIcon])}},

            % Template static files
            {"/css/[...]", cowboy_static, {dir, filename:join([ThemeDir, <<"css">>])}},
            {"/dist/[...]", cowboy_static, {dir, filename:join([ThemeDir, <<"dist">>])}},
            {"/font/[...]", cowboy_static, {dir, filename:join([ThemeDir, <<"font">>])}},
            {"/images/[...]", cowboy_static, {dir, filename:join([ThemeDir, <<"images">>])}},
            {"/js/[...]", cowboy_static, {dir, filename:join([ThemeDir, <<"js">>])}},
            {"/templates/[...]", cowboy_static, {dir, filename:join([ThemeDir, <<"templates">>])}},

            % API
            {"/_utils/api/[...]", wh_api, []},

            % Websocket
            {"/_ws/:instance_id/[...]", wh_websocket, []},

            % Frontend router
            {"/[...]", wh_frontend, FrontendProps},

            {'_', wh_not_found, undefined}
        ]}
    ]),

    Opts = [
        {ip, BindIp},
        {port, BindPort}
    ],

    SslOpts = [
        {certfile, CertFile},
        {keyfile, KeyFile}
    ],

    RouterSchema = #{
        env => #{dispatch => Dispatch},
        middlewares => Middlewares
    },

    case UseTls of
        true -> cowboy:start_tls(https_agent, Opts ++ SslOpts, RouterSchema);
        false -> cowboy:start_clear(http_agent, Opts, RouterSchema)
    end.

%% Local functions
