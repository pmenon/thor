-module(thor_api).

%% Server API
-export([user_login/1,
         send_message/2, 
         get_messages/1, 
         wait_for_messages/1,
         wait_for_messages/2,
         user_logout/1]).

-define(DEFAULT_TIMEOUT, 30*1000).

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
user_login(UserId) ->
    thor_channel_server:create_channel(UserId).

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
send_message(ToUserId, Msg) ->
    thor_channel_server:deliver_to_channel(ToUserId, {chat_msg, Msg}).

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
get_messages(UserId) ->
    thor_channel_server:read_from_channel(UserId).

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
wait_for_messages(UserId) ->
    wait_for_messages(UserId, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
wait_for_messages(UserId, Timeout) ->
    ReqPid = self(),
    thor_channel_server:deliver_to_channel(UserId, {add_listener, ReqPid}),
    Response = receive
        Messages when is_list(Messages) ->
            io:format("~p for messages from channel ~p~n", [ReqPid, Messages]),
            Messages;
        _ ->
            io:format("~p not sure what it got~n", [ReqPid]),
            {error, bad_data_returned}
    after 
        Timeout ->
            io:format("~p timed out while waiting for messages~n", [ReqPid]),
            timeout
    end,
    thor_channel_server:deliver_to_channel(UserId, {remove_listener, ReqPid}),
    Response.

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
user_logout(UserId) ->
    thor_channel_server:destroy_channel(UserId).
