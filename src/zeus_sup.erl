-module(zeus_sup).

-behaviour(supervisor).

%% Include files

%% Exported functions

-export([
    start_link/0,
    init/1
]).

%% API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ZeusSession = {
        zeus_session,
        {zeus_session, start_link, []},
        permanent, 2000, worker,
        [zeus_session]
    },
    ZeusDb = {
        zeus_db,
        {zeus_db, start_link, []},
        permanent, 2000, worker,
        [zeus_db]
    },
    ZeusWeb = {
        zeus_web_server,
        {zeus_web_server, start_link, []},
        permanent, 2000, worker,
        [zeus_web_server]
    },
    {ok, {{one_for_one, 0, 1}, [ZeusSession, ZeusDb, ZeusWeb]}}.

%% Local functions
