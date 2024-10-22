-module(test).
-behaviour(gen_server).

%% API
-export([start_link/0, increment/0, get_count/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% Starts the gen_server
start_link() ->
    gen_server:start_link({local, test}, test, [], []).

%% Client functions
increment() ->
    gen_server:cast(test, increment).

get_count() ->
    gen_server:call(main, get_count).

stop() ->
    gen_server:call(main, stop).

%% gen_server callback functions
init([]) ->
    {ok, 0}.  %% Initial state is 0

handle_call(get_count, _From, State) ->
    {reply, State, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(increment, State) ->
    {noreply, State + 1}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
