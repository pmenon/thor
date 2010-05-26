-module(thor_server).

-behaviour(gen_server).

-include("thor.hrl").

%% API
-export([start_link/1, create/2]).

%% Gen Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Our state
-record(state, {listen_socket,      %% The socket listening on the port
                port,               %% The port the socket is listening on
                acceptor_pid}).     %% The current PID of the accepting thread

start_link(Port) when is_integer(Port) ->
    Name = list_to_atom(lists:flatten(io_lib:format("thor_server_~w", [Port]))),
    gen_server:start_link({local, Name}, ?MODULE, Port, []).

create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create}).

%% Gen Server init callback
init(Port) ->
    ?LOG_INFO("Attempting to listen on port ~w~n", [Port]),
    %%process_flag(trap_exit, true),
    case gen_tcp:listen(Port, [binary, {packet, http},
                                {reuseaddr, true},
                                {active, false},
                                {backlog, 30}]) of
        {ok, ListenSocket} ->
            ?LOG_INFO("Listening on port ~w~n", [Port]),
            %% Create accepting thread
            Pid = thor_socket:start_link(self(), ListenSocket, Port),
            {ok, #state{listen_socket = ListenSocket,
                        port = Port,
                        acceptor_pid = Pid}};
        {error, Reason} ->
            ?LOG_ERROR("Error: There was an error listening on port ... ~p~n", [Reason]),
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({create}, #state{listen_socket = ListenSocket} = State) ->
    NewPid = thor_socket:start_link(self(), ListenSocket, State#state.port),
    {noreply, State#state{acceptor_pid = NewPid}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, normal}, #state{acceptor_pid = Pid} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, abnormal}, #state{acceptor_pid = Pid} = State) ->
    timer:sleep(2000),
    ?LOG_ERROR("Process ~p failed abnormally, attempting to restart~n", [Pid]),
    thor_socket:start_link(self(), State#state.listen_socket, State#state.port),
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_DEBUG("Info request: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


