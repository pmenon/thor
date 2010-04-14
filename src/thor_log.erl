-module(thor_log).
-behaviour(gen_server).

%% API
-export([start_link/1, log/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% This logger's state
-record(state, {fd}).

start_link(Conf) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Conf, []).

log(DebugLevel, Msg) ->
    Caller = self(),
    gen_server:cast(?SERVER, {log, Caller, DebugLevel, Msg}).

init(Conf) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({log, CallerPid, DebugLevel, Msg}, State) ->
    io:format("~p : ~w~n", [CallerPid, Msg]).

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    %% TODO: Need to close log file
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

