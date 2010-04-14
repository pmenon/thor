-module(thor).
-behaviour(application).

%% API
-export([start/0]).

%% application behaviour callbacks
-export([start/2, stop/1]).

start() ->
    io:format("in thor:start/0~n", []),
    application:start(thor).

start(_Type, _StartArgs) ->
    print_banner(),
    io:format("Starting Thor ...~n"),
    case thor_sup:start_link() of 
        {ok, Pid} ->
            %%alarm_handler:clear_alarm({application_stopped, thor}),
            {ok, Pid};
        Error ->
            %%alarm_handler:set_alarm({{application_stopped, thor}, []}),
            Error
    end.

stop(_State) ->
    alarm_handler:set_alarm({{application_stopped, thor}, []}).


print_banner() ->
    io:format("  _____  _             ____      ~n"
              " |_   _|| |__    ___  |  _ \\    ~n"
              "   | |  | '_ \\  / _ \\ | |_) |   ~n"
              "   | |  | | | || (_) ||  _ <     ~n"
              "   |_|  |_| |_| \\___/ |_| \\_\\ ~n", []).

