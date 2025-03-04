-module(zeus_agent_roles).

%% Include files

%% Exported functions

-export([
    is_zeus_administrator/1,
    roles/1,
    is_forbidden/2,
    user/1
]).

%% API

-spec is_zeus_administrator(cowboy_req:req()) -> boolean().

is_zeus_administrator(Req) ->
    IsAdmin = cowboy_req:header(<<"x-zeus-administrator">>, Req),
    IsAdmin =:= <<"true">>.

-spec roles(cowboy_req:req()) -> [zeus_shared_protocol:role()] | 'undefined'.

roles(Req) ->
    case cowboy_req:header(<<"x-zeus-roles">>, Req) of
        undefined ->
            undefined;
        <<>> ->
            case is_zeus_administrator(Req) of
                true -> [admin];
                false -> []
            end;
        Bin ->
            Roles1 = [binary_to_atom(Role, latin1) || Role <- binary:split(Bin , <<", ">>)],
            case is_zeus_administrator(Req) of
                true -> ordsets:from_list([admin | Roles1]);
                false -> Roles1
            end
    end.

-spec is_forbidden(cowboy_req:req(), [zeus_shared_protocol:role()] | 'undefined') -> boolean().

is_forbidden(Req, AllowedRoles) ->
    case roles(Req) of
        undefined -> false;
        Roles -> aplists:intersection(AllowedRoles, Roles) =:= []
    end.

-spec user(cowboy_req:req()) -> binary() | 'undefined'.

user(Req) ->
    cowboy_req:header(<<"x-zeus-user">>, Req).

%% Local functions
