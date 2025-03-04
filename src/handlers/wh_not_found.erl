-module(wh_not_found).

%% Include files

%% Exported functions

-export([
    init/2
]).

%% API

init(Req, Opts) ->
    Req2 = cowboy_req:reply(404, #{}, <<"404 Not Found">>, Req),
    {ok, Req2, Opts}.

%% Local functions
