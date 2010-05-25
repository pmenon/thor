-module(thor_channel).

-include("thor.hrl").

%% API
-export([new_channel/1, start/1, loop/1, send_flush_msg/1]).

-record(state, { name,               %% channel name
                 delivered_msgs,     %% ordered list of delivered messages
                 pending_msgs,       %% ordered list of messages penging delivery
                 listener_pids       %% listeners listening on this channel
                }).

-define(FLUSH_PERIOD, 50000).

%%--------------------------------------------------------------------
%% Function: new_channel(Name)
%% Description: Creates a new channel.  A channel is a general purpose
%%              communication thread that can receive messages from 
%%              other processes and notify listeners upon certain events.
%%--------------------------------------------------------------------
new_channel(Name) ->
    proc_lib:spawn_link(?MODULE, start, [Name]).

%%--------------------------------------------------------------------
%% Function: start(Name)
%% Description: This is the function that is called when creating a new
%%              channel.  It firsts set's up a timer to flush messages
%%              from the queue to some persistent store, then heads
%%              into the receive-loop waiting for messages and sleeping 
%%              in between
%%--------------------------------------------------------------------
start(Name) ->
    timer:apply_interval(?FLUSH_PERIOD, ?MODULE, send_flush_msg, [self()]), 
    proc_lib:hibernate(?MODULE, loop, [#state{name = Name,
                                              delivered_msgs = [],
                                              pending_msgs = [],
                                              listener_pids = []}]).

%%--------------------------------------------------------------------
%% Function: send_flush_msg(ChannelPid)
%% Description: This function is called at a set interval to force
%%              the channel with Pid == ChannelPid to flush any
%%              already delivered messages to some persistence store.
%%--------------------------------------------------------------------
send_flush_msg(ChannelPid) ->
    ChannelPid ! flush.

%%--------------------------------------------------------------------
%% Function: loop(State)
%% Description: This is the channels main receive-loop that carries state.
%%              Rather than just wait on the receive, the channel actually
%%              calls proc_lib:hibernate after processing messages to
%%              preserve space.  This is better done when one does not
%%              expect the channel to receive many messages frequently.
%%--------------------------------------------------------------------
loop(#state{name = Name, listener_pids = Listeners} = State) ->
    receive
        {msg, {chat_msg, Msg}} ->
            %% new message arrived, add to pending, attempt to notify, move to received
            ?LOG_DEBUG("Channel ~p received a message: ~p~n", [Name, Msg]),
            NewPending = lists:append(State#state.pending_msgs, [Msg]),
            NewState = try_notify(State#state{pending_msgs = NewPending}),
            proc_lib:hibernate(?MODULE, loop, [NewState]);
        {msg, {get_msgs, Pid}} ->
            %% send pending messages, move to received
            ?LOG_DEBUG("~p sending messages to ~p~n", [Name, Pid]),
            proc_lib:hibernate(?MODULE, loop, [State]);
        {msg, {add_listener, Pid}} ->
            %% add to listener list
            NewListeners = case lists:member(Pid, Listeners) of
                true ->
                    Listeners;
                false ->
                    [Pid | Listeners]
                end,
            ?LOG_DEBUG("Channel ~p to notify listeners ...~n", [Name]),
            NewState = try_notify(State#state{listener_pids = NewListeners}),
            proc_lib:hibernate(?MODULE, loop, [NewState]);
        {msg, {remove_listener, Pid}} ->
            %% remove from listener list
            NewListeners = lists:filter(fun(P) -> P /= Pid end, Listeners),
            proc_lib:hibernate(?MODULE, loop, [State#state{listener_pids = NewListeners}]);
        flush ->
            ?LOG_DEBUG("Channel ~p is flushing messages ...~n", [Name]),
            %% do_flush(State)
            proc_lib:hibernate(?MODULE, loop, [State]);
        quit ->
            ?LOG_DEBUG("Channel ~p received \"quit\" message ... ~n", [Name]),
            ok;
        _ ->
            %% reject message, go back to sleep
            proc_lib:hibernate(?MODULE, loop, [State])
    end.

%%--------------------------------------------------------------------
%% Function: try_notify(State)
%% Description: This function attempts to notify/deliver any undelivered 
%%              in the channels message queue.  If there are no listeners
%%              this function returns.  If anyone is listening, any pending
%%              messages are delivered asynchronously.  
%%
%%              TODO: configure whether we should do synchronous or asyn
%%                    message delivery as this should be left upto the user.
%%--------------------------------------------------------------------
try_notify(#state{delivered_msgs = DMsgs, pending_msgs = PMsgs, listener_pids = Listeners} = State) ->
    case Listeners of
        [] ->
            %% no listeners to delivery messages to
            ?LOG_DEBUG("No listeners, add to pending ~p~n", [PMsgs]),
            State;
        _ ->
            case PMsgs of 
                [] ->
                    ?LOG_DEBUG("No pending msgs to send ...~n", []),
                    State;
                _ ->
                    ?LOG_DEBUG("Listeners = ~p~n", [Listeners]),
                    %% found listeners, deliver messages and move to delivered
                    lists:map(fun(Pid) ->
                        ?LOG_DEBUG("Sending message ~p to listener ~p~n", [PMsgs, Pid]),
                        Pid ! {msgs, PMsgs}
                    end, 
                    Listeners),
                    NewDelivered = lists:append(DMsgs, PMsgs),
                    State#state{delivered_msgs = NewDelivered, pending_msgs = []}
            end
    end.
