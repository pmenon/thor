-module(thor_file_appender).

-behaviour(gen_event).

-include("thor.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(conf, {fd,
               format,
               dir,
               name,
               suffix,
               rotate,
               size}).

init({conf, ConfArgs}) ->
    Args = lists:foldl(fun(X, Acc) ->
                           [proplists:get_value(X, ConfArgs) | Acc]
                       end, [], [format, dir, log_name, suffix, rotate, size]),
    init(list_to_tuple(lists:reverse(Args)));
init({Format, Dir, LogName, LogSuffix, Rotate, Size}) ->
    File = Dir ++ "/" ++ LogName ++ "." ++ LogSuffix,
    io:format("File = ~p~n", [File]),
    {ok, Fd} = file:open(File, [write, raw, binary]),
    io:format("opened file~n", []),
    {ok, #conf{fd = Fd, format = Format, dir = Dir, name = LogName, suffix = LogSuffix, rotate = Rotate, size = Size}}. 

handle_event({log, Log}, State) ->
    io:format("Logged to file~n", []),
    file:write(State#conf.fd, Log#log.msg),
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


