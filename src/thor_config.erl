-module(thor_config).
-behaviour(gen_server).

-include("thor.hrl").

%%API
-export([start_link/1, get/1, set/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(conf, {conf}).

start_link(ConfFiles) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ConfFiles], []).

get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

set(Key, Value) ->
    gen_server:cast(?SERVER, {set, Key, Value}).

init(ConfFiles) ->
    io:format("Reading configuration file: ~p~n", [ConfFiles]),
    Terms = lists:foldl(fun(ConfFile, Acc) ->
                            Terms = case file:consult(ConfFile) of
                                        {ok, T} -> T;
                                        _ -> []
                                    end,
                            lists:append(Acc, Terms)
                        end, [], ConfFiles),
    {ok, #conf{conf = Terms}}.

get_key(Key, []) ->
    {error, not_found};
get_key(Key, [{Key, Value} | Rest]) ->
    {ok, Value};
get_key(Key, [{_Key, _Value} | Rest]) ->
    get_key(Key, Rest).

handle_call({get, Key}, From, State) ->
    Reply = get_key(Key, State#conf.conf),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set, Key, Value}, State) ->
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

