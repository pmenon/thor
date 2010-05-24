-module(thor_sup).
-behaviour(supervisor).

-include("thor.hrl").

%% supervisor behaviour callbacks
-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Port = 8080,
    HttpServer = {thor_server, {thor_server, start_link, [Port]},
              permanent, 2000, worker, [thor_server]},
    ChannelServer = {thor_channel_server, {thor_channel_server, start_link, []},
                     permanent, 2000, worker, [thor_channel_server]},
    Log = {thor_log, {thor_log, start_link, [?DEFAULT_LOGGER]},
            permanent, 2000, worker, [thor_log]},

    {ok, {{one_for_one, 10, 1}, [HttpServer, ChannelServer, Log]}}.

