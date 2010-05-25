-module(thor_channel_server).
-behaviour(gen_server).

-include("thor.hrl").

%% API
-export([start_link/0, 
         create_channel/1, 
         deliver_to_channel/2,
         read_from_channel/1,
         destroy_channel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {channel_map}).

start_link() ->
    ?LOG_INFO("Starting the Thor Channel Server ... ~n", []),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_channel(ChannelName) ->
    gen_server:cast(?SERVER, {create_channel, ChannelName}).

deliver_to_channel(ChannelName, Msg) -> 
    gen_server:cast(?SERVER, {deliver_msg, ChannelName, Msg}).

read_from_channel(ChannelName) ->
    gen_server:call(?SERVER, {read_from_channel, ChannelName}).

destroy_channel(ChannelName) ->
    gen_server:cast(?SERVER, {destroy_channel, ChannelName}).

init([]) ->
    {ok, #state{channel_map = dict:new()}}.

%%handle_call({read_from_channel, ChannelName}, #state{channel_map = ChannelMap} = State) ->
%%    case dict:find(ChannelName, Channelmap) of
%%        {ok, Channel} ->
%%            io:format("Reading from channel ~p~n", [Channel]),
%%            ChannelPid ! {msg

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({create_channel, ChannelName}, #state{channel_map = ChannelMap} = State) ->
    NewChannelMap = case dict:find(ChannelName, ChannelMap) of
        {ok, Channel} ->
            ?LOG_INFO("Channel already exists for name ~p at ~p~n", [ChannelName, Channel]),
            ChannelMap;
        _ ->
            ChannelPid = thor_channel:new_channel(ChannelName),
            ?LOG_INFO("Creating new channel with name: ~p on ~p~n", [ChannelName, ChannelPid]),
            dict:store(ChannelName, ChannelPid, State#state.channel_map)
    end,
    {noreply, State#state{channel_map = NewChannelMap}};

handle_cast({deliver_msg, ChannelName, Msg}, #state{channel_map = ChannelMap} = State) ->
    case dict:find(ChannelName, ChannelMap) of
        {ok, Channel} ->
            %% send to channel
            ?LOG_DEBUG("Sending message ~p to user ~p on channel ~p~n", [Msg, ChannelName, Channel]),
            Channel ! {msg, Msg};
        _ ->
            ?LOG_DEBUG("No channel for user ~p exists ~n", [ChannelName])
    end,
    {noreply, State};

handle_cast({destroy_channel, ChannelName}, #state{channel_map = ChannelMap} = State) ->
    NewChannelMap = case dict:find(ChannelName, ChannelMap) of
        {ok, Channel} ->
            %% send quit code
            Channel ! quit,
            dict:erase(ChannelName, ChannelMap);
        _ ->
            ChannelMap
    end,
    {noreply, State#state{channel_map = NewChannelMap}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

