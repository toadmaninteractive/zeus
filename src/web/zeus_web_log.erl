-module(zeus_web_log).

-behaviour(cowboy_middleware).

%% Include files

-include_lib("kernel/include/logger.hrl").

%% Exported functions

-export([
    execute/2
]).

%% API

execute(Req, Env) ->
    Url = cowboy_req:uri(Req),
    Method = cowboy_req:method(Req),
    ?LOG_INFO(#{what => zeus_web_log, method => Method, url => Url}),
    {ok, Req, Env}.

%% Local functions
