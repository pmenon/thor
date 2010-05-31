-module(thor_file_appender).

-behaviour(gen_event).

-include("thor.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(file_appender, {fd,
                        level,
                        format,
                        dir,
                        name,
                        suffix,
                        rotate,
                        rotate_num,
                        size,
                        counter}).

init({conf, ConfArgs}) ->
    Args = lists:foldl(fun(X, Acc) ->
                           [proplists:get_value(X, ConfArgs) | Acc]
                       end, [], [level, format, dir, log_name, suffix, rotate, size]),
    init(list_to_tuple(lists:reverse(Args)));
init({Level, Format, Dir, LogName, LogSuffix, {Rotate, RotateNum}, Size}) ->
    File = Dir ++ "/" ++ LogName ++ "." ++ LogSuffix,
    io:format("Logging to file ~p~n", [File]),
    {ok, Fd} = file:open(File, [write, raw, binary]),
    {ok, #file_appender{fd = Fd, level = Level, format = Format, dir = Dir, name = LogName, suffix = LogSuffix, rotate = Rotate, rotate_num = RotateNum, size = Size, counter = 0}};
init({file, ConfFile}) ->
    {ok, Terms} = file:consult(ConfFile),
    init({conf, Terms}).

handle_event({log, Log}, Conf) ->
    NewState =
    case thor_utils:should_log(Log#log.level, Conf#file_appender.level) of
        true -> do_log(Log, Conf);
        false -> Conf
    end,
    NewState2 = 
    case should_rotate(NewState) of
        true -> do_rotate(Conf);
        false -> NewState
    end,
    {ok, NewState2};

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

should_rotate(Conf) ->
    case Conf#file_appender.rotate of
        true -> Conf#file_appender.counter > Conf#file_appender.size;
        false -> false
    end.

do_log(Log, Conf) ->
    Msg = thor_utils:format_log(Log),
    Size = string:len(Msg) + Conf#file_appender.counter,
    file:write(Conf#file_appender.fd, Msg),
    Conf#file_appender{counter = Size}.

do_rotate(#file_appender{fd = Fd, dir = Dir, name = Name, suffix = Suffix,  rotate_num = Num} = Conf) ->
   file:close(Fd),
   File = Dir ++ "/" ++ Name,
   rotate_file(File, Num - 1, Suffix),
   {ok, Fd2} = file:open(File ++ "." ++ Suffix, [write, raw, binary]),
   Conf#file_appender{fd = Fd2, counter = 0}.

rotate_file(File, Num, Suffix) when Num > 0 ->
    file:rename(File ++ "_" ++ integer_to_list(Num) ++ "." ++ Suffix,
                File ++ "_" ++ integer_to_list(Num + 1) ++ "." ++ Suffix),
    rotate_file(File, Num - 1, Suffix);
rotate_file(File, _Num, Suffix) ->
    file:rename(File ++ "." ++ Suffix,
                File ++ "_1." ++ Suffix).
