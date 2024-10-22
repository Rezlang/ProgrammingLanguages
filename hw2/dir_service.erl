-module(dir_service).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Starts the gen_server
start_link(Args) ->
    gen_server:start_link({local, dir_service}, dir_service, Args, []).

init({Num_file_servers, R}) ->
    Pids = spawn_file_server(Num_file_servers, []),
    {ok, #{pids => Pids, r => R, num_servers => Num_file_servers}}.

assign_next_pid([First | Rest]) ->
    assign_next_pid(Rest, First).

assign_next_pid([Current | [Next | Rest]], First) ->
    Current ! {next_pid, Next},
    assign_next_pid([Next | Rest], First);

assign_next_pid([Last], First) ->
    Last ! {next_pid, First}.

spawn_file_server(0, Pids) ->
    assign_next_pid(Pids),
    Pids;

spawn_file_server(N, Pids) ->
    {ok, Pid} = file_server:start_link({"fs" ++ integer_to_list(N-1)}),
    Pids = [Pid | Pids],
    spawn_file_server(N-1, Pids).

handle_cast({create, File_name, File_contents}, State) ->
    File_hash = util:hashFileName(File_name, maps:get(num_servers, State)),
    Pids_tuple = list_to_tuple(maps:get(pids, State)),
    Pid = element(File_hash + 1, Pids_tuple),
    Pid ! {create, {File_name, File_contents, maps:get(r, State)}},
    {ok, State};
    
handle_cast({inactive, File_server}, State) ->
        File_number = list_to_integer(string:substr(File_server, 3)),
        Pids_tuple = list_to_tuple(maps:get(pids, State)),
        Pid = element(File_number + 1, Pids_tuple),
        Pid ! {inactive, {}},
        {ok, State};

handle_cast({quit, _}, State) ->
    terminate("Manual quit", State).


handle_call({request, File_name}, From , State) -> 
    File_hash = util:hashFileName(File_name, maps:get(num_servers, State)),
    Pids_tuple = list_to_tuple(maps:get(pids, State)),
    Pid = element(File_hash + 1, Pids_tuple),
    Pid ! {request, {File_name, maps:get(r, State), From}},
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.
