-module(dir_service).

%% gen_server callbacks
-export([init/1, handle_command/2, terminate/2]).

init({Num_file_servers, R}) ->
    util:write_log("dir_service:init~n"),
    register(dir_pid, self()),

    spawn_file_servers(Num_file_servers, Num_file_servers, util:isDistributed()),
    util:write_log("dir_service ready for commands"),
    command_loop(#{r => R, num_servers => Num_file_servers}).

command_loop(S) ->
    receive
        Arg ->
            {noreply, State} = handle_command(Arg, S),
            command_loop(State)
    end.

%% Spawn the file servers and register each one by name
spawn_file_servers(0, _,  _) ->
    ok;
spawn_file_servers(N, Total_File_Servers, true) ->
    Name = "fs" ++ integer_to_list(N - 1),
    NameAtom = list_to_atom(Name),
    Node = list_to_atom("fs" ++ integer_to_list(N - 1) ++ "@localhost"),
    erlang:spawn_link(Node, fiser, init, [NameAtom, N, Total_File_Servers]),
    util:write_log("dir_service:spawn_file_server/" ++ atom_to_list(NameAtom)),
    spawn_file_servers(N - 1, Total_File_Servers, true);
spawn_file_servers(N, Total_File_Servers, false) ->
    Name = "fs" ++ integer_to_list(N - 1),
    NameAtom = list_to_atom(Name),
    erlang:spawn_link(fiser, init, [NameAtom, N, Total_File_Servers]),
    util:write_log("dir_service:spawn_file_server/" ++ atom_to_list(NameAtom)),
    spawn_file_servers(N - 1, Total_File_Servers, false).


%% Handle Erlang messages
handle_command({create, File_name, File_contents}, State) ->
    util:write_log("dir_service:handle_info/create~n"),
    File_hash = util:hashFileName(File_name, maps:get(num_servers, State)),
    PidName = "fs" ++ integer_to_list(File_hash),
    util:send({create, {File_name, File_contents, maps:get(r, State)}}, PidName, util:isDistributed()),
    {noreply, State};

handle_command({inactive, Fs}, State) ->
    File_server = atom_to_list(Fs),
    util:write_log("dir_service:handle_info/inactive~n"),
    util:send({inactive, {}}, File_server, util:isDistributed()),
    {noreply, State};

handle_command({quit, _}, State) ->
    util:write_log("dir_service:handle_info/quit~n"),
    Num_file_servers = maps:get(num_servers, State),
    propogate_kill(Num_file_servers, State),
    {noreply, State};

handle_command({request, File_name}, State) ->
    util:write_log("dir_service:handle_info/request~n"),
    File_hash = util:hashFileName(File_name, maps:get(num_servers, State)),
    Name = "fs" ++ integer_to_list(File_hash),
    util:send({request, {File_name, maps:get(r, State)}}, Name, util:isDistributed()),
    {noreply, State}.

terminate(_Reason, State) ->
    {stop, normal, State}.

propogate_kill(0, State) ->
    terminate("Manual quit", State);
propogate_kill(N, State) ->
    PidName = list_to_atom("fs" ++ integer_to_list(N)),
    util:send(quit, PidName, util:isDistributed()),
    propogate_kill(N-1, State).
