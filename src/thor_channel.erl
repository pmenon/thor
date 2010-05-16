-module(thor_channel).

%% API
-export([new_channel/0, start/0, loop/1, send_flush_msg/1]).

-record(state, { delivered_msgs,     %% ordered list of delivered messages
                 pending_msgs,       %% ordered list of messages penging delivery
                 listener_pids       %% listeners listening on this channel
                }).

-define(FLUSH_PERIOD, 50000).

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
new_channel() ->
    proc_lib:spawn_link(?MODULE, start, []).

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
start() ->
    timer:apply_interval(?FLUSH_PERIOD, ?MODULE, send_flush_msg, [self()]), 
    proc_lib:hibernate(?MODULE, loop, [#state{delivered_msgs = [],
                                              pending_msgs = [],
                                              listener_pids = []}]).

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
send_flush_msg(ChannelPid) ->
    ChannelPid ! flush.

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
loop(#state{listener_pids = Listeners} = State) ->
    io:format("Channel ~p received a message ... ~n", [self()]),
    receive
        {msg, {chat_msg, Msg}} ->
            %% new message arrived, add to pending, attempt to notify, move to received
            io:format("Channel ~p received chat message: ~p~n", [self(), Msg]),
            NewPending = lists:append(State#state.pending_msgs, [Msg]),
            NewState = try_notify(State#state{pending_msgs = NewPending}),
            proc_lib:hibernate(?MODULE, loop, [NewState]);
        {msg, {get_msgs, Pid}} ->
            %% send pending messages, move to received
            io:format("~p sending messages to ~p~n", [self(), Pid]),
            proc_lib:hibernate(?MODULE, loop, [State]);
        {msg, {add_listener, Pid}} ->
            %% add to listener list
            NewListeners = case lists:member(Pid, Listeners) of
                true ->
                    Listeners;
                false ->
                    [Pid | Listeners]
                end,
            io:format("Channel ~p attempting notification ...~n", [self()]),
            NewState = try_notify(State#state{listener_pids = NewListeners}),
            proc_lib:hibernate(?MODULE, loop, [NewState]);
        {msg, {remove_listener, Pid}} ->
            %% remove from listener list
            NewListeners = lists:filter(fun(P) -> P /= Pid end, Listeners),
            proc_lib:hibernate(?MODULE, loop, [State#state{listener_pids = NewListeners}]);
        flush ->
            io:format("Channel ~p is flushing messages ...~n", [self()]),
            %% do_flush(State)
            proc_lib:hibernate(?MODULE, loop, [State]);
        quit ->
            io:format("Channel ~p received \"quit\" message ... ~n", [self()]),
            ok;
        _ ->
            %% reject message, go back to sleep
            proc_lib:hibernate(?MODULE, loop, [State])
    end.

%%--------------------------------------------------------------------
%% Function: create_channel(Connection, Request)
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
try_notify(#state{delivered_msgs = DMsgs, pending_msgs = PMsgs, listener_pids = Listeners} = State) ->
    case Listeners of
        [] ->
            %% no listeners to delivery messages to
            io:format("no listeners, add to pending ~p~n", [PMsgs]),
            State;
        _ ->
            case PMsgs of 
                [] ->
                    io:format("No pending msgs to send ...~n", []),
                    State;
                _ ->
                    io:format("listeners = ~p~n", [Listeners]),
                    %% found listeners, deliver messages and move to delivered
                    lists:map(fun(Pid) ->
                        io:format("sending message ~p to listener ~p~n", [PMsgs, Pid]),
                        Pid ! {msgs, PMsgs}
                    end, 
                    Listeners),
                    NewDelivered = lists:append(DMsgs, PMsgs),
                    State#state{delivered_msgs = NewDelivered, pending_msgs = []}
            end
    end.
