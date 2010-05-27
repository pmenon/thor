-module(thor_utils).

-include("thor.hrl").

-export([should_log/2, format_log/1]).

%% debug < info < warn < error
should_log(MsgLevel, LogLevel) ->
    case LogLevel of
        debug ->
            true;
        info ->
            MsgLevel =:= info orelse MsgLevel =:= warn orelse MsgLevel =:= error orelse MsgLevel =:= fatal;
        warn ->
            MsgLevel =:= warn orelse MsgLevel =:= error orelse MsgLevel =:= fatal;
        error ->
            MsgLevel =:= error orelse MsgLevel =:= fatal;
        fatal ->
            MsgLevel =:= fatal;
         _ ->
            true
    end.

format_log(#log{time = RealTime, msg = ParamMsg, args = Args, pid = Pid, level = Level} = Log) ->
    STime = format_time(RealTime),
    SLevel = string:to_upper(atom_to_list(Level)),
    SMsg = io_lib:format(ParamMsg, Args),
    T = "[~s] [~s] ~p ~s",
    io_lib:format(T, [STime, SLevel, Pid, SMsg]).

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


