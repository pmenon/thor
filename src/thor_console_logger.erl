-module(thor_console_logger).

-behaviour(gen_event).

-include("thor.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(console_appender, {}).

init(InitArgs) ->
    {ok, #console_appender{}}.    

handle_event({log, Log}, State) ->
    Time = format_time(Log#log.time),
    Level = string:to_upper(atom_to_list(Log#log.level)),
    io:format("[~s] [~s] ~s~n", [Time, Level, Log#log.msg]),
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

format_time(Time) ->
    {{Y, Mon, D}, {H, Min, S}} = Time,
    [FY, FMon, FD, FH, FMin, FS] = lists:map(fun(X) ->
                                                 X1 = integer_to_list(X),
                                                 case string:len(X1) of
                                                    1 -> "0" ++ X1;
                                                    _ -> X1
                                                 end
                                            end, [Y, Mon, D, H, Min, S]),
    FY++"-"++FMon++"-"++FD++" "++FH++":"++FMin++":"++FS.

