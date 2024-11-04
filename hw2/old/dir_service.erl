-module(dir_service).

%% gen_server callbacks
-export([dir_init/1, dir_handle_command/2, dir_terminate/2]).

dir_init({Num_file_servers, R}) ->
    util:write_log("dir_service:init~n"),
    register(dir_pid, self()),

    dir_spawn_file_servers(Num_file_servers, Num_file_servers, util:isDistributed()),
    util:write_log("dir_service ready for commands"),
    dir_command_loop(#{r => R, num_servers => Num_file_servers}).

dir_command_loop(S) ->
    receive
        Arg ->
            {noreply, State} = dir_handle_command(Arg, S),
            dir_command_loop(State)
    end.

%% Spawn the file servers and register each one by name
dir_spawn_file_servers(0, _,  _) ->
    ok;
dir_spawn_file_servers(N, Total_File_Servers, true) ->
    Name = "fs" ++ integer_to_list(N - 1),
    NameAtom = list_to_atom(Name),
    Node = list_to_atom("fs" ++ integer_to_list(N - 1) ++ "@localhost"),
    erlang:spawn_link(Node, fiser, init, [NameAtom, N, Total_File_Servers]),
    util:write_log("dir_service:spawn_file_server/" ++ atom_to_list(NameAtom)),
    dir_spawn_file_servers(N - 1, Total_File_Servers, true);
dir_spawn_file_servers(N, Total_File_Servers, false) ->
    Name = "fs" ++ integer_to_list(N - 1),
    NameAtom = list_to_atom(Name),
    erlang:spawn_link(fiser, init, [NameAtom, N, Total_File_Servers]),
    util:write_log("dir_service:spawn_file_server/" ++ atom_to_list(NameAtom)),
    dir_spawn_file_servers(N - 1, Total_File_Servers, false).


%% Handle Erlang messages
dir_handle_command({create, File_name, File_contents}, State) ->
    util:write_log("dir_service:handle_info/create~n"),
    File_hash = util:hashFileName(File_name, maps:get(num_servers, State)),
    PidName = "fs" ++ integer_to_list(File_hash),
    util:send({create, {File_name, File_contents, maps:get(r, State)}}, PidName, util:isDistributed()),
    {noreply, State};

dir_handle_command({inactive, Fs}, State) ->
    File_server = atom_to_list(Fs),
    util:write_log("dir_service:handle_info/inactive~n"),
    util:send({inactive, {}}, File_server, util:isDistributed()),
    {noreply, State};

dir_handle_command({quit, _}, State) ->
    util:write_log("dir_service:handle_info/quit~n"),
    Num_file_servers = maps:get(num_servers, State),
    dir_propogate_kill(Num_file_servers, State),
    {noreply, State};

dir_handle_command({request, File_name}, State) ->
    util:write_log("dir_service:handle_info/request~n"),
    File_hash = util:hashFileName(File_name, maps:get(num_servers, State)),
    Name = "fs" ++ integer_to_list(File_hash),
    util:send({request, {File_name, maps:get(r, State)}}, Name, util:isDistributed()),
    {noreply, State}.

dir_terminate(_Reason, State) ->
    {stop, normal, State}.

dir_propogate_kill(0, State) ->
    dir_terminate("Manual quit", State);
dir_propogate_kill(N, State) ->
    PidName = list_to_atom("fs" ++ integer_to_list(N)),
    util:send(quit, PidName, util:isDistributed()),
    dir_propogate_kill(N-1, State).
