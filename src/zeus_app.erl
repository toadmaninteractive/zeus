-module(zeus_app).

-behaviour(application).

%% Include files

%% Exported functions

-export([
    start/2,
    stop/1
]).

%% API

start(_Type, _StartArgs) ->
    zeus_sup:start_link().

stop(_State) ->
    ok.

%% Local functions
