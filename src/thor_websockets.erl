-module(thor_websockets).

-include("thor.hrl").

-export([init/3, close/1, unpack/1, send_data/2, get_data/1, set_passive/1, set_active/1]).
-export([is_websocket_request/1]).

-define(ORIGIN_HEADER, "Origin").
-define(UPGRADE_HEADER, "Upgrade").
-define(WEBSOCKET_UPGRADE, "WebSocket").

-record(opts, {callback,
               owner_pid,
               active}).

parse_options(Options) ->
    parse_options(Options, #opts{}).

parse_options([{callback, Callback} | Rest], Opts) ->
    parse_options(Rest, Opts#opts{callback = Callback});
parse_options([{owner_pid, OwnerPid} | Rest], Opts) ->
    parse_options(Rest, Opts#opts{owner_pid = OwnerPid});
parse_options([{active, Active} | Rest], Opts) ->
    parse_options(Rest, Opts#opts{active = Active});
parse_options([_ | Rest], Opts) ->
    parse_options(Rest, Opts);
parse_options([], Opts) ->
    Opts.

init(#conn{sock = WebSocket} = Conn, #req{headers = Headers} = Request, Options) ->
    NewOptions = parse_options(Options),
    handshake(WebSocket, Headers, NewOptions),
    do_callback(NewOptions#opts.callback, WebSocket).

handshake(WebSocket, Headers, Options) ->
    case get_origin_header(Headers) of
        undefined ->
            close(WebSocket);
        Origin ->
            Response = 
                ["HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
		         "Upgrade: WebSocket\r\n",
		         "Connection: Upgrade\r\n",
		         "WebSocket-Origin: ", Origin, "\r\n",
		         "WebSocket-Location: ws://localhost:8080/thor\r\n",
		         "\r\n"],
            

            gen_tcp:send(WebSocket, Response),
            inet:setopts(WebSocket, [{packet, raw}, {active, Options#opts.active}]),
            gen_tcp:controlling_process(WebSocket, Options#opts.owner_pid)
    end.

close(WebSocket) ->
    send_data(WebSocket, []),
    gen_tcp:close(WebSocket).

do_callback(Callback, Args) ->
    Callback(Args).

send_data(WebSocket, Data) ->
    FramedData = [0, Data, 255],
    io:format("sending data: ~p~n", [FramedData]),
    gen_tcp:send(WebSocket, [0,Data,255]).

get_data(WebSocket) ->
    Res = gen_tcp:recv(WebSocket, 0),
    case Res of
        {ok, Data} ->
            unpack(Data);
        {error, _Reason} ->
            closed
    end.

set_options(WebSocket, Options) when is_list(Options) ->
    inet:setopts(WebSocket, Options);
set_options(WebSocket, _Options) ->
    {error, bad_options_format}.

set_passive(WebSocket) ->
    set_options(WebSocket, [{active, false}]).

set_active(WebSocket) ->
    set_options(WebSocket, [{active, true}]).

unpack(FramedData) ->
    unpack_data([], binary_to_list(FramedData)).
unpack_data(Data, [0|Rest]) ->
    unpack_data(Data, Rest);
unpack_data(Data, [255]) ->
    lists:reverse(Data);
unpack_data(Data, [H|Rest]) ->
    unpack_data([H | Data], Rest).

get_origin_header(Headers) ->
    thor_httprequest:get_header_val(Headers, ?ORIGIN_HEADER).

get_upgrade_header(Headers) ->
    thor_httprequest:get_header_val(Headers, ?UPGRADE_HEADER).

is_websocket_request(Request) ->
    Val = get_upgrade_header(Request#req.headers),
    case Val of
        ?WEBSOCKET_UPGRADE -> true;
        undefined -> false;
        _ -> false
    end.
