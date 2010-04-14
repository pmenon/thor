-module(thor_router).

-export([get_route_handler/3]).

get_route_handler(Port, Method, Path) ->
    io:format("getting router for path = ~p~n", [Path]),
    {ok, thor_channel_server, do_something}.
