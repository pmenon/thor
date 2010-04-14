-module(thor_web).

-export([do_loop/2]).

-define(COMET_TIMEOUT, 30*1000).

%%--------------------------------------------------------------------
%% Function: do_loop(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
do_loop(Connection, Request) ->
    io:format("~p handling request for path ~p~n", [self(), Request#req.uri]),
    Headers = ["Server: Thor"],
    Body = binary_to_list(Request#req.body),
    case Request#req.uri of
        {abs_path, "/create"} ->
            thor_channel_server:create_channel(Body),
            Reply = {200, Headers, <<"created channel">>};
        {abs_path, "/send"} ->
            {User, Message} = get_data(Body),
            JsonMsg = {struct, [ {from, User}, {msg, Message} ]},
            thor_channel_server:deliver_to_channel(User, {chat_msg, JsonMsg}),
            Reply = {200, Headers, <<"Message sent!\r\n\r\n">>};
        {abs_path, "/get"} ->
            Msg = {add_listener, self()},
            {User, Message} = get_data(Body),
            thor_channel_server:deliver_to_channel(User, Msg),
            timer:apply_after(30000, ?MODULE, timeout, []),
            proc_lib:hibernate(?MODULE, handle_msgs, []);
            %receive
            %    Stuff ->
            %        RemoveMsg = {remove_listener, self()},
            %        thor_channel_server:deliver_to_channel(User, RemoveMsg),
            %        io:format("messages : ~p~n", [Stuff]),
            %        Reply = {200, Headers, list_to_binary(thor_json:encode({array, Stuff}))}
            %after 30000 ->
            %    RemoveMsg = {remove_listener, self()},
            %    thor_channel_server:deliver_to_channel(User, RemoveMsg),
            %    io:format("timeout~n", []),
            %    Reply = {200, Headers, <<"timed out, try again">>}
            %end;
        _ ->            
            Body = <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n",
             "<html>\n",
             "    <head>\n",
             "        <title>Welcome to Thor!</title>\n",
             "    </head>\n",
             "    <body>\n",
             "        Hello, World!\n",
             "    </body>\n",
             "</html>\n">>,
             Reply = {200, Headers, Body}
    end,
    Reply.

handle_request(Connection, "/thor/get", Request) ->
    {User, Message} = get_data(Body),
    thor_channel_server:deliver_to_channel( User, { add_listener, self() } ),
    timer:apply_after(?COMET_TIMEOUT, ?MODULE, timeout, []),
    proc_lib:hibernate(?MODULE, handle_msgs, []);

handle_request(Connection, _Path, Request) ->
    %% unsupported operation

finish_wait() ->
    thor_channel_server:deliver_to_channel(User, {remove_listener, self()}),
    
wait(Connection) ->
    receive
        timeout ->
            io:format("~p received timeout message, please try again later~n", [self()]);
        Msgs when is_list(Msgs) ->
            io:format("~p received new messages : ~p~n", [self(), Msgs])
    end.

timeout_wait(Pid) ->
    Pid ! timeout.


