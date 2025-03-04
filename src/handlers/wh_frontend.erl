-module(wh_frontend).

%% Include files

-include("settings.hrl").

%% Exported functions

-export([
    init/2
]).

%% API

init(Req, State) ->
    % Check session
    Cookies = cowboy_req:parse_cookies(Req),
    SessionId = proplists:get_value(?SESSION_ID_VAR, Cookies, undefined),
    IsAuthenticated = zeus_session:touch(SessionId) =:= ok,
    Path = cowboy_req:path(Req),
    Req2 = route(#route{path = Path, authenticated = IsAuthenticated, session_id = SessionId}, Req, State),
    {ok, Req2, State}.

%% Local functions

layout(Req, State) ->
    ThemeDir = proplists:get_value(theme_dir, State, undefined),
    Production = proplists:get_value(production, State, true),
    {ok, Template} = file:read_file(path_to_index(ThemeDir, Production)),
    cowboy_req:reply(200, #{}, Template, Req).

route(#route{authenticated = false}, Req, State) ->
    layout(Req, State);
route(#route{authenticated = true, session_id = SessionId}, Req, State) ->
    Req2 = zeus_util:prolong_session(SessionId, Req),
    layout(Req2, State).

path_to_index(ThemeDir, true) ->
    filename:join([ThemeDir, <<"dist">>, <<"index.html">>]);
path_to_index(ThemeDir, _) ->
    filename:join([ThemeDir, <<"index.html">>]).
