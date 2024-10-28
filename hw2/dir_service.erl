-module(dir_service).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Starts the gen_server
start_link(Args) ->
    gen_server:start_link({global, dir_pid}, dir_service, Args, []).
init({Num_file_servers, R}) ->
    util:write_log("dir_service:init~n"),
    global:register_name(dir_pid, self()),
    NodeList = [list_to_atom("fs" ++ integer_to_list(N) ++ "@localhost") || N <- lists:seq(0, Num_file_servers - 1)],
    %% Connect to each file server node
    [ begin 
        case net_adm:ping(Node) of
          pong ->
            util:write_log("Pinged " ++ atom_to_list(Node));
          pang ->
            util:write_log("Failed to ping " ++ atom_to_list(Node))
        end
    end || Node <- NodeList],
    timer:sleep(1000), %% Wait for nodes to be fully connected
    PidList = spawn_file_servers(Num_file_servers, []),
    assign_next_pid(PidList),
    {ok, #{r => R, num_servers => Num_file_servers}}.

%% Spawn the file servers and register each one by name
spawn_file_servers(0, PidList) ->
    util:write_log("dir_service:spawn_file_servers/0~n"),
    lists:reverse(PidList);
spawn_file_servers(N, PidList) ->
    Name = list_to_atom("fs" ++ integer_to_list(N - 1)),
    Node = list_to_atom("fs" ++ integer_to_list(N - 1) ++ "@localhost"),
    {ok, NewPid} = rpc:call(Node, fiser, start_link, [Name]),
    _ = rpc:call(Node, global, register_name, [Name, NewPid]),

    % util:write_log(erlang:display(registered())) ,
    spawn_file_servers(N - 1, [NewPid | PidList]).

assign_next_pid(PidList) ->
    Pids = PidList,
    PidListWithNext = lists:zip(Pids, lists:append(tl(Pids), [hd(Pids)])),
    lists:foreach(fun({CurrentPid, NextPid}) ->
        gen_server:cast(CurrentPid, {next_pid, NextPid})
    end, PidListWithNext),
    ok.
    

handle_call(_,_, State) ->
    {noreply, State}.

handle_cast({create, File_name, File_contents}, State) ->
    util:write_log("dir_service:handle_cast/create~n"),
    File_hash = util:hashFileName(File_name, maps:get(num_servers, State)),
    PidName = list_to_atom("fs" ++ integer_to_list(File_hash)),
    Pid = util:get_global_pid(3,PidName),
    gen_server:cast(Pid, {create, {File_name, File_contents, maps:get(r, State)}}),
    {noreply, State};

handle_cast({inactive, File_server}, State) ->
    util:write_log("dir_service:handle_cast/inactive~n"),
    Pid = util:get_global_pid(3,File_server),
    gen_server:cast(Pid, {inactive, {}}),
    {noreply, State};

handle_cast({quit, _}, State) ->
    util:write_log("dir_service:handle_cast/quit~n"),
    Num_file_servers = maps:get(num_servers, State),
    propogate_kill(Num_file_servers, State);

handle_cast({request, File_name, From}, State) -> 
    util:write_log("dir_service:handle_cast/request~n"),
    File_hash = util:hashFileName(File_name, maps:get(num_servers, State)),
    PidName = list_to_atom("fs" ++ integer_to_list(File_hash)),
    Pid = util:get_global_pid(3,PidName),
    gen_server:cast(Pid, {request, {File_name, maps:get(r, State), From}}),
    {noreply, State}.

terminate(_Reason, State) ->
    {stop, normal, State}.

propogate_kill(0, State) ->
    terminate("Manual quit", State);
propogate_kill(N, State) ->
    PidName = list_to_atom("fs" ++ integer_to_list(N)),
    Pid = util:get_global_pid(3,PidName),
    gen_server:cast(Pid, quit),
    propogate_kill(N-1, State).
