-module(thor_sup).
-behaviour(supervisor).

-include("thor.hrl").

%% supervisor behaviour callbacks
-export([start_link/1, init/1]).

-define(SERVER, ?MODULE).

start_link(DefaultConfigFile) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [DefaultConfigFile]).

init(DefaultConfigFiles) ->
    ConfigFile = get_config_file(DefaultConfigFiles),
    thor_config:start_link(ConfigFile),

    %%Config = {thor_config, {thor_config, start_link, [ConfigFile]},
    %%          permanent, 2000, worker, [thor_config]},
    Log = {thor_log, {thor_log, start_link, [?DEFAULT_LOGGER]},
            permanent, 2000, worker, [thor_log]},

    {ok, Port} = thor_config:get(port),
    io:format("port = ~p~n", [Port]),
    HttpServer = {thor_server, {thor_server, start_link, [Port]},
              permanent, 2000, worker, [thor_server]},

    ChannelServer = {thor_channel_server, {thor_channel_server, start_link, []},
                     permanent, 2000, worker, [thor_channel_server]},

    {ok, {{one_for_one, 10, 1}, [Log, HttpServer, ChannelServer]}}.

get_config_file(Default) ->
    case init:get_argument(conf) of
    error ->
        Default;
    {ok, [[]]} ->
        Default;
    {ok, [Values]} ->
        Values
    end.
    
