-module(thor_web).

-include("thor.hrl").

-export([handle_request/2]).

-define(COMET_TIMEOUT, 30*1000).

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------

handle_request(Connection, "/thor/get_messages", #req{body = Body} = Request) ->
    Headers = ["Server: Thor Web Server!"],
    {User, Message} = get_data(binary_to_list(Body)),
    Response = thor_api:wait_for_messages(User),
    HttpResp = case Response of
        Messages when is_list(Messages) ->
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
    io:format("Body = ~p, User = ~p, Message = ~p~n", [binary_to_list(Body), User, Message]),
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
    {User, MsgTxt} = get_data(binary_to_list(Body)),
    JsonMsg = {struct, [ {from, User}, {msg, MsgTxt} ]},
    thor_api:send_message(User, JsonMsg),
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
    handle_request(Connection, Path, Request).


get_data(Data) ->
    get_data(Data, []).

get_data([$:|T], Acc) ->
    {lists:reverse(Acc), T};
get_data([H|T], Acc) ->
    get_data(T, [H|Acc]);
get_data([], Acc) ->
    {lists:reverse(Acc), []}.
