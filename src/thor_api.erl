-module(thor_api).

-include("thor.hrl").

%% Server API
-export([user_login/1,
         send_message/2, 
         get_messages/1, 
         wait_for_messages/1,
         wait_for_messages/2,
         add_observer/2,
         remove_observer/2,
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
        {msgs, Messages} when is_list(Messages) ->
            ?LOG_DEBUG("Received messages from channel: ~p~n", [Messages]),
            {msgs, Messages};
        _ ->
            ?LOG_WARN("Received unknown data while waiting for messages~n", []),
            {error, bad_data_returned}
    after 
        Timeout ->
            ?LOG_INFO("Timed out while waiting for messages~n", []),
            timeout
    end,
    thor_channel_server:deliver_to_channel(UserId, {remove_listener, ReqPid}),
    Response.

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
add_observer(UserId, ObserverPid) ->
    thor_channel_server:deliver_to_channel(UserId, {add_listener, ObserverPid}).

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
remove_observer(UserId, ObserverPid) ->
    thor_channel_server:deliver_to_channel(UserId, {remove_listener, ObserverPid}).

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
user_logout(UserId) ->
    thor_channel_server:destroy_channel(UserId).
