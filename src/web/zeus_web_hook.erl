-module(zeus_web_hook).

-behaviour(cowboy_middleware).

%% Include files

%% Exported functions

-export([
    execute/2
]).

%% API

execute(Req, Env) ->
    case cowboy_req:path(Req) of
        <<"templates/", _/binary>> ->
            Req2 = no_cache(Req),
            {ok, Req2, Env};
        _ ->
            {ok, Req, Env}
    end.

%% Local functions

no_cache(Req) ->
    Req1 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-cache, no-store, must-revalidate">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"pragma">>, <<"no-cache">>, Req1),
    cowboy_req:set_resp_header(<<"expires">>, <<"0">>, Req2).
