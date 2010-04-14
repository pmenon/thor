-module(thor_channel_server).
-behaviour(gen_server).

-include("thor.hrl").

%% API
-export([start_link/0, create_channel/1, deliver_to_channel/2, destroy_channel/1]).
-export([do_something/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {user_map}).

start_link() ->
    io:format("Starting the Thor Channel Server~n", []),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

deliver_to_channel(UserId, Msg) -> 
    gen_server:cast(?SERVER, {deliver_msg, UserId, Msg}).

create_channel(UserId) ->
    gen_server:cast(?SERVER, {create_channel, UserId}).

destroy_channel(UserId) ->
    gen_server:cast(?SERVER, {destroy_channel, UserId}).

init([]) ->
    {ok, #state{user_map = dict:new()}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({create_channel, UserId}, #state{user_map = UserMap} = State) ->
    NewUserMap = case dict:find(UserId, UserMap) of
        {ok, Channel} ->
            io:format("Channel already exists for user ~p~n", [UserId]),
            UserMap;
        _ ->
            ChannelPid = thor_channel:new_channel(),
            io:format("Creating new channel for user ~p on ~p~n", [UserId, ChannelPid]),
            dict:store(UserId, ChannelPid, State#state.user_map)
    end,
    {noreply, State#state{user_map = NewUserMap}};

handle_cast({deliver_msg, UserId, Msg}, State) ->
    case dict:find(UserId, State#state.user_map) of
        {ok, Channel} ->
            %% send to channel
            io:format("sending message ~p to user ~p on channel ~p~n", [Msg, UserId, Channel]),
            Channel ! {msg, Msg};
        _ ->
            io:format("no channel for user ~p exists ~n", [UserId])
    end,
    {noreply, State};

handle_cast({destroy_channel, UserId}, #state{user_map = UserMap} = State) ->
    NewUserMap = case dict:find(UserId, UserMap) of
        {ok, Channel} ->
            %% send quit code
            Channel ! quit,
            dict:erase(UserId, State#state.user_map);
        _ ->
            UserMap
    end,
    {noreply, State#state{user_map = NewUserMap}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

wait() ->
    io:format("woke up~n", []),
    receive
        timeout ->
            io:format("timeout~n", []);
        Stuff ->
            io:format("stuff = ~p~n", [Stuff])
    end.

timeout_wait(Pid) ->
    Pid ! timeout.

do_something(Connection, Request, Argument) ->
    io:format("~p handling request for path ~p~n", [self(), Request#req.uri]),
    Headers = ["Server: Thor"],
    Body = binary_to_list(Request#req.body),
    case Request#req.uri of
        {abs_path, "/create"} ->
            thor_channel_server:create_channel(Body),
            Reply = {200, Headers, <<"created channel">>};
        {abs_path, "/send"} ->
            {User, Message} = get_data(Body),
            JsonMsg = {struct, [ {from, User}, {msg, Message} ]},
            thor_channel_server:deliver_to_channel(User, {chat_msg, JsonMsg}),
            Reply = {200, Headers, <<"Message sent!\r\n\r\n">>};
        {abs_path, "/get"} ->
            Msg = {add_listener, self()},
            {User, Message} = get_data(Body),
            thor_channel_server:deliver_to_channel(User, Msg),
            receive
                Stuff ->
                    RemoveMsg = {remove_listener, self()},
                    thor_channel_server:deliver_to_channel(User, RemoveMsg),
                    io:format("messages : ~p~n", [Stuff]),
                    Reply = {200, Headers, list_to_binary(thor_json:encode({array, Stuff}))}
            after 30000 ->
                RemoveMsg = {remove_listener, self()},
                thor_channel_server:deliver_to_channel(User, RemoveMsg),
                io:format("timeout~n", []),
                Reply = {200, Headers, <<"timed out, try again">>}
            end;
        _ ->            
            Body = <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n",
             "<html>\n",
             "    <head>\n",
             "        <title>Welcome to Thor!</title>\n",
             "    </head>\n",
             "    <body>\n",
             "        Hello, World!\n",
             "    </body>\n",
             "</html>\n">>,
             Reply = {200, Headers, Body}
    end,
    Reply.

get_data(Data) ->
    get_data(Data, []).

get_data([$:|T], Acc) ->
    {lists:reverse(Acc), T};
get_data([H|T], Acc) ->
    get_data(T, [H|Acc]);
get_data([], Acc) ->
    {lists:reverse(Acc), []}.
