-module(thor_console_appender).

-behaviour(gen_event).

-include("thor.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(console_appender, {format,
                           level}).

init({conf, ConfArgs}) ->
    Args = lists:foldl(fun(T, Acc) ->
                           [proplists:get_value(T, ConfArgs) | Acc]
                       end, [], [format, level]),
    init(list_to_tuple(Args));
init({Level}) ->
    init({Level, ""});
init({Level, Format}) ->
    {ok, #console_appender{format = Format, level = Level}}.

handle_event({log, Log}, State) ->
    NewState = 
    case thor_utils:should_log(Log#log.level, State#console_appender.level) of
        true -> do_log(Log, State);
        false -> State
    end,
    {ok, NewState};

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

do_log(Log, State) ->
    Time = format_time(Log#log.time),
    Level = string:to_upper(atom_to_list(Log#log.level)),
    Msg = io_lib:format(Log#log.msg, Log#log.args),
    io:format("[~s] [~s] ~p ~s", [Time, Level, Log#log.pid, Msg]),
    State.


