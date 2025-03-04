-module(zeus_agent_xml).

%% Include files

-include_lib("kernel/include/logger.hrl").
-include("zeus_shared.hrl").

%% Exported functions

-export([
    create/0,
    clear/0,
    layout/2,
    xml_to_json/1,
    xml_to_json/2,
    parse_xml/1
]).

%% API

create() ->
    ets:new(?MODULE, [public, named_table, set, {keypos, 1}]),
    ok.

clear() ->
    ets:delete_all_objects(?MODULE),
    ok.

-spec layout(binary(), [binary()]) -> binary().

layout(RootFolder, UrlPath) when is_list(UrlPath), is_binary(RootFolder) ->
    FileKey = make_path(RootFolder, UrlPath),
    case filelib:last_modified(FileKey) of
        0 ->
            throw(not_found);
        FileDatetime ->
            case ets:lookup(?MODULE, FileKey) of
                {_, Datetime, Json} when FileDatetime =:= Datetime ->
                    Json;
                _ ->
                    try
                        %% ?LOG_DEBUG("xml->json ~s", [FileKey]),
                        Json = xml_to_json(FileKey),
                        ets:insert(?MODULE, {FileKey, FileDatetime, Json}),
                        Json
                    catch
                        Error:Reason:Stacktrace ->
                            ?LOG_ERROR(#{error => Error, reason => Reason, stacktrace => Stacktrace}),
                            throw(internal_server_error)
                    end
            end
    end.

-spec xml_to_json(binary()) -> binary().

xml_to_json(Path) ->
    Layout = parse_xml(Path),
    Json = zeus_shared_protocol:element_layout_to_json(Layout),
    zeus_util:encode(Json).

-spec xml_to_json(binary(), [binary()]) -> binary().

xml_to_json(RootFolder, UrlPath) ->
    xml_to_json(make_path(RootFolder, UrlPath)).

-spec parse_xml(binary()) -> #element_layout{}.

parse_xml(Path) ->
    parse_root(decode_file(Path)).

%% Local functions

make_path(RootFolder, UrlPath) ->
    IsBadRequest = [0 || <<"..">> <- UrlPath] =/= [],
    if IsBadRequest -> throw(bad_request); true -> ignore end,
    Path = filename:join([RootFolder | UrlPath]),
    <<Path/binary, ".xml">>.

decode_file(Path) ->
    case xmerl_scan:file(Path, [{comments, false}]) of
        {error, Error} -> error(Error);
        {Xml, []} -> Xml
    end.

parse_root(Xml) ->
    zeus_shared_protocol:element_layout_from_xml(Xml).
