-module(zeus_util).

%% Include files

-include("settings.hrl").
-include("zeus_server.hrl").

%% Exported functions

-export([
    encode/1,
    decode/1,
    cerberus_authenticate/2,
    cerberus_authenticate/3,
    cerberus_realms/0,
    to_binary/1,
    timestamp/0,
    timestamp/1,
    timestamp_to_datetime/1,
    iso_8601_fmt/1,
    reset_session/1,
    prolong_session/2,
    reply_body/4,
    reply_body_status/5,
    reply_json/4,
    reply_igor/4,
    json_body/1,
    http_method/1,
    pattern_match/2,
    validate/2,
    binary_join/2,
    http_req/4,
    error_to_binary/1
]).

%% API

-spec encode(Json :: jsx:json_term()) -> jsx:json_text().

encode(Json) ->
    jsx:encode(Json).

-spec decode(Binary :: jsx:json_text()) -> jsx:json_term().

decode(Binary) ->
    jsx:decode(Binary, [return_maps]).

-spec cerberus_authenticate(User, Pass) -> 'ok' | {'error', Reason :: term()} when
    User :: binary() | string(),
    Pass :: binary() | string().

cerberus_authenticate(User, Pass) ->
    {ok, Realms} = application:get_env(zeus, authentication_realms),
    cerberus_authenticate(User, Pass, hd(Realms)).

-spec cerberus_authenticate(User, Pass, Realm) -> 'ok' | {'error', Reason :: term()} when
    User :: binary() | string(),
    Pass :: binary() | string(),
    Realm :: atom().

cerberus_authenticate(User, Pass, Realm) ->
    try
        cerberus:authenticate(Realm, User, Pass)
    of
        ok -> ok;
        {reject, Reason} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    catch
        _:Reason -> {error, Reason}
    end.

-spec cerberus_realms() -> [{Realm :: atom(), Title :: binary()}].

cerberus_realms() ->
    {ok, Realms} = application:get_env(zeus, authentication_realms),
    [{Realm, to_binary(cerberus:title(Realm))} || Realm <- Realms].

-spec to_binary(X) -> binary() when
    X :: binary() | string() | integer() | float() | atom().

to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_integer(X) -> to_binary(integer_to_list(X));
to_binary(X) when is_float(X) -> to_binary(float_to_list(X));
to_binary(X) when is_atom(X) -> to_binary(atom_to_list(X)).

-spec timestamp() -> non_neg_integer().

timestamp() ->
    {Mega, Sec, Micro} = erlang:timestamp(),
    Mega * 100000000000 + Sec * 100000 + Micro.

-spec timestamp(DateTime :: calendar:datetime()) -> non_neg_integer().

timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.

-spec timestamp_to_datetime(Timestamp :: non_neg_integer()) -> calendar:datetime().

timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp + 62167219200).

-spec iso_8601_fmt(calendar:datetime() | {'datetime', calendar:datetime()}) -> binary().

iso_8601_fmt({datetime, DateTime}) ->
    iso_8601_fmt(DateTime);
iso_8601_fmt({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    IoList = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec]),
    iolist_to_binary(IoList).

-spec reset_session(Req :: cowboy_req:req()) -> cowboy_req:req().

reset_session(Req) ->
    cowboy_req:set_resp_cookie(?SESSION_ID_VAR, <<>>, Req, #{path => <<"/">>, max_age => 0}).

-spec prolong_session(SessionId :: binary(), Req :: cowboy_req:req()) -> cowboy_req:req().

prolong_session(SessionId, Req) ->
    cowboy_req:set_resp_cookie(?SESSION_ID_VAR, SessionId, Req, #{path => <<"/">>, max_age => ?SESSION_LIFETIME}).

-spec reply_body(Method, Body, Req, State) -> Result when
    Method :: atom(),
    Body :: iodata(),
    Req :: cowboy_req:req(),
    State :: term(),
    Result :: {Reply, Req1 :: cowboy_req:req(), State1 :: term()},
    Reply :: boolean() | binary().

reply_body(Method, Body, Req, State) when Method =:= get; Method =:= head ->
    {Body, Req, State};
reply_body(Method, Body, Req, State) when Method =:= put; Method =:= post; Method =:= patch ->
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {true, Req2, State};
reply_body(Method, Body, Req, State) when Method =:= delete ->
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {true, Req2, State}.

-spec reply_body_status(Status, Headers, Body, Req, State) -> Result when
    Status :: cowboy:http_status(),
    Headers :: cowboy:http_headers(),
    Body :: iodata(),
    Req :: cowboy_req:req(),
    State :: term(),
    Result :: {'stop', cowboy_req:req(), term()}.

reply_body_status(Status, Headers, Body, Req, State) ->
    Req1 = cowboy_req:reply(Status, Headers, Body, Req),
    {stop, Req1, State}.

-spec reply_json(Method, Json, Req, State) -> Result when
    Method :: atom(),
    Json :: list() | maps:map(),
    Req :: cowboy_req:req(),
    State :: term(),
    Result :: {Reply, Req1 :: cowboy_req:req(), State1 :: term()},
    Reply :: boolean() | binary().

reply_json(Method, Json, Req, State) ->
    reply_body(Method, encode(Json), Req, State).

-spec reply_igor(Method, IgorRec, Req, State) -> Result when
    Method :: atom(),
    IgorRec :: #reply_status{},
    Req :: cowboy_req:req(),
    State :: term(),
    Result :: {Reply, Req1 :: cowboy_req:req(), State1 :: term()},
    Reply :: boolean() | binary().

reply_igor(Method, IgorRec, Req, State) ->
    Json = zeus_server_protocol:reply_status_to_json(IgorRec),
    reply_json(Method, Json, Req, State).

-spec json_body(Req :: cowboy_req:req()) -> Result when
    Result :: {jsx:json_term() | 'undefined', Req2 :: cowboy_req:req()}.

json_body(Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            case read_body(Req, <<>>) of
                {ok, Body, Req1} ->
                    try
                        Json = zeus_util:decode(Body),
                        {Json, Req1}
                    catch
                        _:_ -> {undefined, Req1}
                    end;
                _ ->
                    {undefined, Req}
            end;
        false ->
            {undefined, Req}
    end.

-spec http_method(Method :: binary()) -> Result when
    Result :: 'get' | 'head' | 'post' | 'put' | 'patch' | 'delete' | 'options' | 'undefined'.

http_method(<<"GET">>) -> get;
http_method(<<"HEAD">>) -> head;
http_method(<<"POST">>) -> post;
http_method(<<"PUT">>) -> put;
http_method(<<"PATCH">>) -> patch;
http_method(<<"DELETE">>) -> delete;
http_method(<<"OPTIONS">>) -> options;
http_method(_) -> undefined.

-spec pattern_match(Subject, Pattern) -> Result when
    Subject :: iodata() | unicode:charlist(),
    Pattern :: re:mp() | iodata(),
    Result :: boolean().

pattern_match(Subject, Pattern) ->
    case re:run(Subject, Pattern) of
        {match, _} -> true;
        _ -> false
    end.

-spec validate(Kind, Param :: term()) -> boolean() when
    Kind :: 'non_empty_string'.

validate(non_empty_string, Param) when is_binary(Param) andalso Param =/= <<>> -> true;
validate(url, Param) when is_binary(Param) -> pattern_match(Param, "^https?\:\/\/[^\/\s]+(\/.*)?$");
validate(_, _) -> false.

-spec binary_join(List :: [binary()], Separator :: binary()) -> binary().

binary_join(List, Separator) ->
    binary_join(List, Separator, <<>>).

-spec http_req(Method, Headers :: proplists:proplist(), Url :: binary(), Body :: binary()) -> Result when
    Method :: 'get' | 'head' | 'post' | 'put' | 'patch' | 'delete' | 'options',
    Result :: {'ok', Status :: non_neg_integer(), Headers1 :: proplists:proplist(), Body :: binary()} | {'error', Reason :: term()}.

http_req(Method, Headers, Url, Content) ->
    case ibrowse:send_req(binary_to_list(Url), Headers, Method, binary_to_list(Content), []) of
        {ok, Status, RHeaders, Body} ->
            RHeaders1 = [{to_binary(Key), to_binary(Value)} || {Key, Value} <- RHeaders],
            {ok, list_to_integer(Status), RHeaders1, to_binary(Body)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec error_to_binary(Error) -> binary() when
    Error :: {'error', Reason} | Reason | term(),
    Reason :: atom().

error_to_binary({error, Reason}) -> error_to_binary(Reason);
error_to_binary(Reason) when is_atom(Reason) -> zeus_util:to_binary(Reason);
error_to_binary(_) -> <<"internal_error">>.

%% Local functions

binary_join([], _, Acc) -> Acc;
binary_join([H|T], Separator, Acc) when Acc =:= <<>> -> binary_join(T, Separator, <<H/binary>>);
binary_join([H|T], Separator, Acc) -> binary_join(T, Separator, <<Acc/binary, Separator/binary, H/binary>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} -> read_body(Req, <<Acc/binary, Data/binary>>)
    end.
