-module(thor_web).

-include("thor.hrl").

-export([handle_request/2]).

-define(COMET_TIMEOUT, 30*1000).
-define(USER_KEY, "user").

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------

handle_request(Connection, "/thor/get_messages", #req{body = Body} = Request) ->
    Headers = ["Server: Thor Web Server!"],
    {User, Message} = get_data(binary_to_list(Body)),
    Response = thor_api:wait_for_messages(User),
    HttpResp = case Response of
        {msgs, Messages} when is_list(Messages) ->
            io:format("wtf?~n", []),
            {200, Headers, list_to_binary(thor_json:encode( {array, Messages} ))};
        timeout ->
            {200, Headers, list_to_binary(thor_json:encode({struct, [ {response, "success"}]}))}; 
        _ ->
            {500, Headers, <<"Error on the server">>}
    end,
    HttpResp;

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
handle_request(Connection, "/thor/login", #req{body = Body} = Request) ->
    Headers = ["Server: Thor Web Server!"],
    {User, Message} = get_data(binary_to_list(Body)),
    thor_api:user_login(User),
    JsonResponse = {struct, [ {response, User} ]},
    {200, Headers, list_to_binary(thor_json:encode(JsonResponse))};

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
handle_request(Connection, "/thor/logout", #req{body = Body} = Request) ->
    Headers = ["Server: Thor Web Server!"],
    {User, Message} = get_data(binary_to_list(Body)),
    thor_api:user_logout(User),
    JsonResponse = {struct, [ {response, "success"} ]},
    {200, Headers, list_to_binary(thor_json:encode(JsonResponse))};

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
handle_request(Connection, "/thor/send_message", #req{body = Body} = Request) ->
    Headers = ["Server: Thor Web Server!"],
    {JsonObj, Rest} = thor_json:decode(binary_to_list(Body)),
    User = thor_json:get_attribute(JsonObj, ?USER_KEY),
    thor_api:send_message(User, JsonObj),
    JsonResponse = {struct, [ {response, "success"} ]},
    {200, Headers, list_to_binary(thor_json:encode(JsonResponse))};

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
handle_request(Connection, _Path, Request) ->
    %% unsupported operation
    Headers = ["Server: Thor Web Server!"],
    Response = <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n",
             "<html>\n",
             "    <head>\n",
             "        <title>Welcome to Thor!</title>\n",
             "    </head>\n",
             "    <body>\n",
             "        Hello, World!\n",
             "    </body>\n",
             "</html>\n">>,
    {200, Headers, Response}.

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
handle_request(Connection, Request) ->
    io:format("~p handling request for path ~p~n", [self(), Request#req.uri]),
    {PathType, Path} = Request#req.uri,
    case thor_websockets:is_websocket_request(Request) of 
        false ->
            handle_request(Connection, Path, Request);
        true ->
            Callback = fun(S) ->
                           websocket_handler(S)
                       end,
            thor_websockets:init(Connection, Request, [ {callback, Callback}, 
                                                        {active, false},
                                                        {owner_pid, self()} ])
    end.

websocket_handler(WebSocket) ->
    Data = thor_websockets:get_data(WebSocket),
    {JsonObj, Rest} = thor_json:decode(Data),
    case thor_json:get_attribute(JsonObj, ?USER_KEY) of
        undefined ->
            thor_websockets:close(WebSocket),
            exit(normal);
        UserName ->
            thor_api:add_observer(UserName, self()),
            thor_websockets:set_active(WebSocket),
            websocket_handler0(WebSocket, UserName)
    end.

websocket_handler0(WebSocket, User) ->
    receive
        {tcp, WebSocket, FramedData} ->
            Data = thor_websockets:unpack(FramedData),
            io:format("got data: ~p~n", [Data]),
            handle_websocket_request(WebSocket, Data);
        {tcp_closed, WebSocket} ->
            io:format("closed websocket~n", []),
            thor_api:remove_observer(User, self()),
            exit(normal);
        {msgs, Message} ->
            io:format("msgs~n", []),
            Json = thor_json:encode({array, Message}),
            thor_websockets:send_data(WebSocket, Json);
        _ ->
            io:format("received unknownd data~n", [])
    end,
    websocket_handler0(WebSocket, User).

handle_websocket_request(WebSocket, Message) ->
    thor_websockets:send_data(WebSocket, Message).

get_data(Data) ->
    get_data(Data, []).

get_data([$:|T], Acc) ->
    {lists:reverse(Acc), T};
get_data([H|T], Acc) ->
    get_data(T, [H|Acc]);
get_data([], Acc) ->
    {lists:reverse(Acc), []}.
