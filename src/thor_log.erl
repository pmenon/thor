-module(thor_log).

-include("thor.hrl").

-export([start_link/1, add_appender/3]).
-export([debug/1, debug/2, info/1, info/2, warn/1, warn/2, error/1, error/2]).

start_link(Logger) ->
    Res = gen_event:start_link({local, Logger}),
    add_appender(Logger, {thor_console_logger, "Console"}, {}),
    Res.

debug(LogMsg) ->
    debug(?DEFAULT_LOGGER, LogMsg).

debug(Logger, LogMsg) ->
    log(Logger, debug, LogMsg).

info(LogMsg) ->
    info(?DEFAULT_LOGGER, LogMsg).

info(Logger, LogMsg) ->
    log(Logger, info, LogMsg).

warn(LogMsg) ->
    warn(?DEFAULT_LOGGER, LogMsg).

warn(Logger, LogMsg) ->
    log(Logger, warn, LogMsg).

error(LogMsg) ->
    error(?DEFAULT_LOGGER, LogMsg).

error(Logger, LogMsg) ->
    log(Logger, error, LogMsg).

log(Logger, Level, LogMsg) ->
    Time = calendar:local_time(),
    {_, _, Millis} = erlang:now(),
    notify(Logger, {log, #log{level= Level, msg = LogMsg, time = Time, millis = Millis}}).

add_appender(Logger, {LogModule, LogName}, Conf) ->
    gen_event:add_sup_handler(Logger, {LogModule, LogName}, Conf).

notify(Logger, LogMessage) ->
    gen_event:notify(Logger, LogMessage).
