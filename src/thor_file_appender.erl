-module(thor_file_appender).

-behaviour(gen_event).

-include("thor.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(conf, {fd,
               level,
               format,
               dir,
               name,
               suffix,
               rotate,
               size}).

init({conf, ConfArgs}) ->
    Args = lists:foldl(fun(X, Acc) ->
                           [proplists:get_value(X, ConfArgs) | Acc]
                       end, [], [level, format, dir, log_name, suffix, rotate, size]),
    init(list_to_tuple(lists:reverse(Args)));
init({Level, Format, Dir, LogName, LogSuffix, Rotate, Size}) ->
    File = Dir ++ "/" ++ LogName ++ "." ++ LogSuffix,
    io:format("Logging to file ~p~n", [File]),
    {ok, Fd} = file:open(File, [write, raw, binary]),
    {ok, #conf{fd = Fd, level = Level, format = Format, dir = Dir, name = LogName, suffix = LogSuffix, rotate = Rotate, size = Size}}. 

handle_event({log, Log}, State) ->
    NewState =
    case thor_utils:should_log(Log#log.level, State#conf.level) of
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

do_log(Log, State) ->
    file:write(State#conf.fd, Log#log.msg ++ "\n"),
    State.
