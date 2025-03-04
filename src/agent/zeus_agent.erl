-module(zeus_agent).

%% Include files

%% Exported functions

-export([
    add_log_to_req/3
]).

%% API

-spec add_log_to_req(Data :: term(), Message :: binary(), cowboy_req:req()) -> cowboy_req:req().

add_log_to_req(Data, Message, Req) ->
    AgentReply = #{message => Message, data => Data},
    cowboy_req:set_resp_header(<<"x-zeus-log">>, base64:encode(zeus_util:encode(AgentReply)), Req).

%% Local functions
