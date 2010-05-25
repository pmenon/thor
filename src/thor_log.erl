-module(thor_log).

-include("thor.hrl").

-export([start_link/1, add_appender/3]).
-export([debug/2, debug/3, info/2, info/3, warn/2, warn/3, error/2, error/3]).

start_link(Logger) ->
    Res = gen_event:start_link({local, Logger}),
    add_appender(Logger, {thor_console_logger, "Console"}, {}),
    Res.

debug(Logger, Msg) ->
    debug(Logger, Msg, []).

debug(Logger, Msg, Args) ->
    log(Logger, debug, Msg, Args).

info(Logger, Msg) ->
    info(Logger, Msg, []).

info(Logger, Msg, Args) ->
    log(Logger, info, Msg, Args).

warn(Logger, Msg) ->
    warn(Logger, Msg, []).

warn(Logger, Msg, Args) ->
    log(Logger, warn, Msg, Args).

error(Logger, Msg) ->
    error(Logger, Msg, []).

error(Logger, Msg, Args) ->
    log(Logger, error, Msg, Args).

log(Logger, Level, Msg, Args) ->
    Time = calendar:local_time(),
    {_, _, Millis} = erlang:now(),
    notify(Logger, {log, #log{level= Level, pid = self(), msg = Msg, args = Args, time = Time, millis = Millis}}).

add_appender(Logger, {LogModule, LogName}, Conf) ->
    gen_event:add_sup_handler(Logger, {LogModule, LogName}, Conf).

notify(Logger, LogMessage) ->
    gen_event:notify(Logger, LogMessage).
