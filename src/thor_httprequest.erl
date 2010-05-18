-module(thor_httprequest).

-export([ok/2, get_header_val/2]).

do_response(Status, Headers, Body) ->
    {Status, Headers, Body}.

ok(ResponseHeaders, Body) ->
    do_response(200, ResponseHeaders, Body).

get_header_val(Headers, HeaderName) when is_list(Headers) ->
    HeaderName0 = string:to_lower(HeaderName),
    lists:foldl(fun({K, V}, undefined) ->
                   K0 = case is_atom(K) of
                            true -> atom_to_list(K);
                            false -> K
                   end,
                   case string:to_lower(K0) of
                       HeaderName0 -> V;
                       _ -> undefined
                   end;
               (_Header, Acc) ->
                   Acc
               end, undefined, Headers).
    
