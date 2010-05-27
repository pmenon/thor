-module(thor_utils).

-include("thor.hrl").

-export([should_log/2]).

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
