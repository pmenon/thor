-module(thor_file_appender).

-behaviour(gen_event).

-include("thor.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(conf, {}).

init(InitArgs) ->
    {ok, #conf{}}.

handle_event({log, Log}, State) ->
    io:format("Logged to file~n", []),
    {ok, State};

handle_event(_Event, State) ->
    io:format("Unknown event~n", []),
    {ok, State}.

handle_call(Request, State) ->
    io:format("Called!", []),
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


