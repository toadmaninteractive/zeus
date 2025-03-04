-module(zeus_igor_custom).

%% Include files

%% Exported functions

-export([
    binding_to_json/1,
    binding_from_json/1,
    json_binding_to_json/1,
    json_binding_from_json/1,
    binding_from_xml/1,
    json_binding_from_xml/1,
    identity/1
]).

%% API

binding_from_xml(Xml) ->
    Value = igor_xml:parse_primitive(Xml),
    parse_binding_text(Value).

parse_binding_text([${, $B, $i, $n, $d, $i, $n, $g | Tail]) ->
    Value = lists:droplast(Tail), % drop }
    Tockens = string:tokens(Value, " "),
    Binding = [list_to_atom(V) || V <- string:tokens(hd(Tockens), ".")],
    case Tockens of
        [_] ->
            {binding, Binding};
        [_, Format] ->
            FormatBin = list_to_binary(string:strip(Format)),
            {binding, Binding, FormatBin}
    end;
parse_binding_text(Text) ->
    list_to_binary(Text).

json_binding_from_xml(Xml) ->
    igor_xml:parse_value(Xml, {map, string, {custom, fun zeus_igor_custom:binding_from_xml/1}}).

binding_to_json({binding, Path}) ->
    #{<<"binding">> => binding_path(Path)};
binding_to_json({binding, Path, Converter}) ->
    #{<<"binding">> => binding_path(Path), <<"converter">> => binding_converter(Converter)};
binding_to_json(Value) when is_number(Value) ->
    #{<<"content">> => Value};
binding_to_json(Value) when is_binary(Value) ->
    #{<<"content">> => Value};
binding_to_json(Value) when is_boolean(Value) ->
    #{<<"content">> => Value};
binding_to_json(Value) when is_atom(Value) ->
    #{<<"content">> => atom_to_binary(Value, latin1)};
binding_to_json(Value) when is_list(Value) ->
    #{<<"content">> => [binding_converter(X) || X <- Value]}.

binding_path(Binding) when is_atom(Binding) ->
    [ atom_to_binary(Binding, latin1) ];
binding_path(Binding) when is_list(Binding) ->
    [ atom_to_binary(Atom, latin1) || Atom <- Binding ].

binding_converter(undefined) -> null;
binding_converter(null) -> null;
binding_converter(X) when is_boolean(X) -> X;
binding_converter(X) when is_binary(X) -> X;
binding_converter(X) when is_atom(X) -> atom_to_binary(X, latin1);
binding_converter(X) when is_number(X) -> X.

binding_from_json(#{<<"binding">> := Binding}) ->
    {binding, [ binary_to_atom(Bin, latin1) || Bin <- Binding ]};
binding_from_json(#{<<"binding">> := Binding, <<"converter">> := Converter}) ->
    {binding, [ binary_to_atom(Bin, latin1) || Bin <- Binding ], Converter};
binding_from_json(#{<<"content">> := Value}) when is_number(Value) ->
    Value;
binding_from_json(#{<<"content">> := Value}) when is_binary(Value) ->
    Value.

json_binding_to_json({binding, Path}) ->
    #{<<"binding">> => binding_path(Path)};
json_binding_to_json(Value) when is_number(Value) ->
    Value;
json_binding_to_json(Value) when is_binary(Value) ->
    Value;
json_binding_to_json(null) ->
    null;
json_binding_to_json(Value) when is_boolean(Value) ->
    Value;
json_binding_to_json(Value) when is_atom(Value) ->
    atom_to_binary(Value, latin1);
json_binding_to_json(List) when is_list(List) ->
    [ json_binding_to_json(V) || V <- List ];
json_binding_to_json(KV) when is_map(KV) ->
    maps:map(fun(_K, V) -> json_binding_to_json(V) end, KV).

json_binding_from_json(#{<<"binding">> := Binding}) ->
    {binding, [ binary_to_atom(Bin, latin1) || Bin <- Binding ]};
json_binding_from_json(Value) when is_number(Value) ->
    Value;
json_binding_from_json(Value) when is_binary(Value) ->
    Value;
json_binding_from_json(null) ->
    null;
json_binding_from_json(Value) when is_boolean(Value) ->
    Value;
json_binding_from_json(List) when is_list(List) ->
    [ json_binding_from_json(V) || V <- List ];
json_binding_from_json(KV) when is_map(KV) ->
    maps:map(fun(_K, V) -> json_binding_from_json(V) end, KV).

-spec identity(T) -> T.

identity(T) -> T.

%% Local functions
