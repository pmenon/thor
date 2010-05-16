-module(thor_json).

%%API
-export([encode/1, decode/1, get_attribute/2]).

encode(Term) ->
    json_encode(Term).

json_encode(true) ->
    <<"true">>;
json_encode(false) ->
    <<"false">>;
json_encode(null) ->
    <<"null">>;
json_encode(I) when is_integer(I) ->
    integer_to_list(I);
json_encode(S) when is_atom(S) ->
    json_encode_string(atom_to_list(S));
json_encode(S) when is_list(S) ->
    case is_string(S) of
        yes -> json_encode_string(S);
        no -> json_encode_string(binary_to_list(S))
    end;
json_encode({array, Array}) when is_list(Array) ->
    json_encode_array(Array);
json_encode({struct, Prop }) when is_list(Prop) ->
    json_encode_proplist(Prop).


json_encode_proplist([]) ->
    <<"{}">>;
json_encode_proplist(Props) ->
    Fun = fun({K, V}, Acc) ->
            KS = json_encode(K),
            VS = json_encode(V),
            [$,, VS, $:, KS | Acc]
          end,
    [$, | Acc] = lists:foldl(Fun, "{", Props),
    lists:reverse(["}" | Acc]).

json_encode_array([]) ->
    <<"[]">>;
json_encode_array(Array) ->
    Fun = fun(E, Acc) ->
            ES = json_encode(E),
            [$,, ES | Acc]
          end,
    [$, | Acc] = lists:foldl(Fun, "[", Array),
    lists:reverse(["]" | Acc]).

json_encode_string(S) ->
    json_encode_string(S, [$"]).

json_encode_string([], Acc) ->
    lists:reverse([$" | Acc]);
json_encode_string([C|Rest], Acc) ->
    json_encode_string(Rest, [C | Acc]).

is_string([]) ->
    yes;
is_string([C|Rest]) when C >= 0, C =< 255 ->
    is_string(Rest);
is_string([_,_]) ->
    no.

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
decode(String) ->
    json_decode(String).

json_decode([$" | String]) ->
    json_decode_string(String);
json_decode([${ | PropList]) ->
    {Struct, Rest} = json_decode_proplist(skipws(PropList)),
    {{struct, Struct}, Rest};
json_decode([$[ | Array]) ->
    {Arr, Rest} = json_decode_array(skipws(Array)),
    {{array, Arr}, Rest}.


json_decode_string(String) ->
    json_decode_string(String, []).
json_decode_string([$" | String], Acc) ->
    %%{list_to_atom(lists:reverse(Acc)), String};
    {lists:reverse(Acc), String};
json_decode_string([Char | Rest], Acc) ->
    json_decode_string(Rest, [Char | Acc]).

json_decode_proplist(PropList) ->
    json_decode_proplist(PropList, []).

json_decode_proplist([$" | Rest], Acc) ->
    {Key, Rest2} = json_decode_string(Rest),
    [$: | Rest3] = skipws(Rest2),
    {Value, Rest4} = json_decode(skipws(Rest3)),
    json_decode_proplist(skipws(Rest4), [{Key, Value} | Acc]);
json_decode_proplist([$, | Rest], Acc) ->
    json_decode_proplist(skipws(Rest), Acc);
json_decode_proplist([$} | Rest], Acc) ->
    {Acc, Rest}.

json_decode_array(Array) ->
    json_decode_array(Array, []).

json_decode_array([$, | Rest], Acc) ->
    json_decode_array(skipws(Rest), Acc);
json_decode_array([$] | Rest], Acc) ->
    {Acc, Rest};
json_decode_array(Rest, Acc) ->
    {Element, Rest1} = json_decode(skipws(Rest)),
    json_decode_array(skipws(Rest1), [Element | Acc]).

skipws([C | Rest]) when C =< 32 ->
    skipws(Rest);
skipws(String) ->
    String.

get_attribute({struct, Props}, Key) ->
    case lists:keysearch(Key, 1, Props) of
        {value, {_Key, Val}} ->
            Val;
        _ ->
            undefined 
    end;
get_attribute(_, Key) ->
    undefined.
