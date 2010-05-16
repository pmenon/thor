-module(thor_socket).

-export([start_link/3, init/1]).

-include("thor.hrl").

-define(server_idle_timeout, 30*1000).

%% common HTTP responses
-define(not_implemented_501, "HTTP/1.1 501 Not Implemented\r\n\r\n").
-define(forbidden_403, "HTTP/1.1 403 Forbidden\r\n\r\n").
-define(not_found_404, "HTTP/1.1 404 Not Found\r\n\r\n").


start_link(ListenPid, ListenSocket, ListenPort) ->
    proc_lib:spawn_link(?MODULE, init, [{ListenPid, ListenSocket, ListenPort}]).

init({ListenPid, ListenSocket, ListenPort}) ->
    io:format("Spawned thread to accept request on port ~p~n", [ListenPort]),
    case catch gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Got a new request, handling it ...~n", []),
            %% tell the listener to spawn a new acceptor while we handle
            %% this incoming request
            thor_server:create(ListenPid, self()),

            %% get client address and port information
            {ok, {Addr, Port}} = inet:peername(Socket),

            %% assemble connection information
            Connection = #conn{sock = Socket,
                               port = ListenPort,
                               peer_addr = Addr,
                               peer_port = Port},

            %% let's start handling this request by reading the request
            request(Connection, #req{});
        Else ->
            error_logger:error_report([{application, thor},
                                       "Accept failed error.",
                                       io_lib:format("~p", [Else])]),
            exit({error, accept_failed})
    end.

%% Reads the HTTP Request Line
request(Connection, Request) ->
    io:format("~p reading new request~n", [self()]),
    case gen_tcp:recv(Connection#conn.sock, 0, 30000) of
        {ok, {http_request, Method, Path, Version}} ->
            %% Proceed to read the HTTP headers
            headers(Connection, Request#req{vsn = Version,
                                           method = Method,
                                           uri = Path}, []);
        {error, {http_error, "\r\n"}} ->
            request(Connection, Request);
        {error, {http_error, "\n"}} ->
            request(Connection, Request);
        _Other ->
            exit(normal)
    end.

%% Reads the HTTP Headers from the request
headers(Connection, Request, Headers) ->
    case gen_tcp:recv(Connection#conn.sock, 0, ?server_idle_timeout) of
        {ok, {http_header, _, 'Content-Length', _, Val}} ->
            Len = list_to_integer(Val),
            headers(Connection, Request#req{content_length = Len}, [{'Content-Length', Val}|Headers]);
        {ok, {http_header, _, 'Connection', _, Val}} ->
            KeepAlive = keep_alive(Request#req.vsn, Val),
            headers(Connection, Request#req{connection = KeepAlive}, [{'Connection', Val}|Headers]);
        {ok, {http_header, _, Header, _, Val}} ->
            io:format("~p: ~p~n", [Header, Val]),
            headers(Connection, Request, [{Header, Val}|Headers]);
        {error, {http_error, "\r\n"}} ->
            headers(Connection, Request, Headers);
        {error, {http_error, "\n"}} ->
            headers(Connection, Request, Headers);
        {ok, http_eoh} ->
            %% move to read the HTTP Body
            body(Connection, Request#req{headers = lists:reverse(Headers)});
        _Other ->
            exit(normal)
    end.

keep_alive({1,1}, "close") -> close;
keep_alive({1,1}, "Close") -> close;
keep_alive({1,1}, _) -> keep_alive;
keep_alive({1,0}, "Keep-Alive") -> keep_alive;
keep_alive({1,0}, _) -> close;
keep_alive({0,9}, _) -> close;
keep_alive(Vsn, KA) ->
    io:format("Got ~p~n", [{Vsn, KA}]),
    close.


%% Reads the HTTP Content/Body from the request
body(Connection, Request) ->
    Body = case Request#req.method of 
        'GET' ->
            <<"">>;    
        'POST' when is_integer(Request#req.content_length) ->
            inet:setopts(Connection#conn.sock, [{packet, raw}]),
            case gen_tcp:recv(Connection#conn.sock, Request#req.content_length, 60000) of
                {ok, Bin} ->
                    Bin;
                _Other ->
                    exit(normal)
            end;
        _Other ->
            send(Connection, ?not_implemented_501),
            exit(normal)
    end,
    Close = case Request#req.method of
        'GET' ->
            handle_get(Connection, Request#req{body = Body});
        'POST' ->
            handle_post(Connection, Request#req{body = Body})
    end,
    case Close of
        close ->
            gen_tcp:close(Connection#conn.sock);
        keep_alive ->
            inet:setopts(Connection#conn.sock, [{packet, http}]),
            request(Connection, #req{})
    end.

handle_get(Connection, #req{connection = Conn} = Req) ->
    case Req#req.uri of
        {abs_path, Path} ->
            {F, Args} = split_at_q_mark(Path),
            call_mfa(Path, Args, Connection, Req),
            Conn;
        {absoluteURI, http, _Host, _, Path} ->
            {F, Args} = split_at_q_mark(Path),
            call_mfa(Path, Args, Connection, Req),
            Conn;
        {absoluteURI, _OtherMethod, _Host, _, _Path} ->
            send(Connection, ?not_implemented_501),
            close;
        {scheme, _Scheme, _RequestString} ->
            send(Connection, ?not_implemented_501),
            close;
        _ ->
            send(Connection, ?forbidden_403),
            close
    end.

handle_post(Connection, #req{connection = Conn} = Req) ->
    case Req#req.uri of
        {abs_path, Path} ->
            call_mfa(Path, Req#req.body, Connection, Req),
            Conn;
        {absoluteURI, http, _Host, _, Path} ->
            call_mfa(Path, Req#req.body, Connection, Req),
            Conn;
        {absoluteURI, _OtherMethod, _Host, _, _Path} ->
            send(Connection, ?not_implemented_501),
            close;
        {scheme, _Scheme, _RequestString} ->
            send(Connection, ?not_implemented_501),
            close;
        _ ->
            send(Connection, ?forbidden_403),
            close
    end.

call_mfa(F, A, Connection, Request) ->
    case thor_router:get_route_handler(Connection#conn.port, Request#req.method, F) of
        {ok, Mod, Func} ->
            case catch Mod:Func(Connection, Request) of
                {'EXIT', Reason} ->
                    io:format("Worked crashed with reason ~p~n", [Reason]),
                    exit(normal);
                {200, Headers, Body} ->
                    ResHeaders = add_content_length(Headers, Body),
                    EncodedHeaders = encode_headers(ResHeaders),
                    Res = [<<"HTTP/1.1 200 OK \r\n">>,
                           EncodedHeaders,
                           <<"\r\n\r\n">>,
                           Body],
                    send(Connection, Res);
                _ ->
                    io:format("sdfsdfsd~n", [])
            end;
        {error, not_found} ->
            send(Connection, ?not_found_404)
    end.

add_content_length(Headers, Body) ->
    case lists:keysearch('Content-Length', 1, Headers) of
        {value, _} ->
            Headers;
        false ->
            [{'Content-Length', size(Body)}|Headers]
    end.

encode_headers([{Tag, Val}|Rest]) when is_atom(Tag) ->
    [atom_to_list(Tag), ": ", encode_header_val(Val), "\r\n" | Rest];
encode_headers([{Tag, Val}|Rest]) when is_list(Tag) ->
    [Tag, ": ", encode_header_val(Val), "\r\n" | Rest];
encode_headers([]) ->
    [].

encode_header_val(Val) when is_atom(Val) ->
    atom_to_list(Val);
encode_header_val(Val) when is_integer(Val) ->
    integer_to_list(Val);
encode_header_val(Val) ->
    Val.

split_at_q_mark(Path) ->
    split_at_q_mark(Path, []).

split_at_q_mark([$?|T], Acc) ->
    {lists:reverse(Acc), T};
split_at_q_mark([H|T], Acc) ->
    split_at_q_mark(T, [H|Acc]);
split_at_q_mark([], Acc) ->
    {lists:reverse(Acc), []}.


send(#conn{sock = Sock} = Connection, Data) ->
    case gen_tcp:send(Sock, Data) of
        ok ->
            io:format("~p sent data okay~n", [self()]),
            ok;
        _ ->
            io:format("~p error sending data .. quitting~n", [self()]),
            exit(normal)
    end.
