-module(thor_router).

-include("thor.hrl").

-export([get_route_handler/3]).

%% TODO: This needs serious refactoring.  Should be configured once.
get_route_handler(Port, Method, Path) ->
    ?LOG_INFO("Getting router for path : ~p~n", [Path]),
    {ok, thor_web, handle_request}.
