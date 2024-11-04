-module(main).

-export([start_dir_service/2, get/1, create/1, deactivate/1, quit/0, dir_init/1, fiser_init/3]).

start_dir_service(N, R) ->
    write_log("start_dir_service"),
    erlang:spawn_link(main, dir_init, [{N, R}]),
    main_client_register().

create(Name) ->
    write_log("client:create"),
    main_client_register(),
    Contents = util:readFile("./input/" ++ Name),
    send({create, Name, Contents}, "dir_pid", util:isDistributed()).


get(Name) ->
    write_log("client:get"),
    main_client_register(),
    
    send({request, Name}, "dir_pid", util:isDistributed()),
    receive
        {request_response, File_contents} ->
            util:saveFile("./output/downloads/" ++ Name, File_contents),
            write_log("Received file contents~n"),
            ok;
        {error, Error_file} ->
            util:saveFile("./output/downloads/" ++ Error_file, ""),
            write_log("Received error file~n"),
            error
    end.

deactivate(Name) ->
    write_log("client:deactivate"),
    send({inactive, Name}, "dir_pid", util:isDistributed()).

quit() ->
    write_log("client:quit"),
    send({quit, nothing}, "dir_pid", util:isDistributed()),
    erlang:halt().

main_client_register() ->
    case whereis(client) of
        undefined -> 
            register(client, self()),
            ok;
        _Pid -> 
            unregister(client),
            register(client, self())
    end.

fiser_init(Name, N, Total_File_Servers) ->
    write_log("fiser:init"),
    register(Name, self()),
    IdNum = N rem Total_File_Servers,
    Id = "fs" ++ integer_to_list(IdNum),
    Folder_path = "./servers/" ++ atom_to_list(Name) ++ "/",
    write_log("fiser: " ++ atom_to_list(Name) ++ " ready for commands, replicating to: " ++ Id),
    fiser_command_loop(#{inactive => false, next_id => Id, folder_path => Folder_path}).

fiser_command_loop(S) ->
    receive
        Arg -> 
            {noreply, State} = fiser_handle_command(Arg, S),
            fiser_command_loop(State)
    end.

fiser_forward_request(_,  File_name, R) when R =< 1->
    send({error, atom_to_list(File_name) ++ ".err"}, "client", util:isDistributed());
fiser_forward_request(NextId, File_name, R) ->
    send({request, {File_name, R - 1}}, NextId, util:isDistributed()).

fiser_forward_create(_, _, _, R) when R =< 1 ->
    ok;
fiser_forward_create(NextId, File_name, File_contents, R) ->
    send({create, {File_name, File_contents, R - 1}}, NextId, util:isDistributed()).
fiser_save_maybe(_, _, true) ->
    ok;
fiser_save_maybe(Path, File_contents, false) ->  
    write_log("fiser:save_maybe/~n"),
    write_log(Path),
    write_log("~n"),
    
    util:saveFile("output/" ++ Path, File_contents).

fiser_handle_command({request, {File_name, R}}, State) ->
    write_log("fiser:handle_call/request~n"),
    case maps:get(inactive, State) of
        true -> 
            NextId = maps:get(next_id, State),
            fiser_forward_request(NextId, File_name, R),
            {noreply, State};

        false -> 
            try
                Path = maps:get(folder_path, State),
                Response = util:readFile("output/" ++ Path ++ File_name),
                send({request_response, Response}, "client", util:isDistributed())
            catch
                error:badmatch -> 
                    fiser_forward_request(maps:get(next_id, State), File_name, R)
            end,
            {noreply, State}
    end;


fiser_handle_command({create, {File_name, File_contents, R}}, State) ->
    write_log("fiser:handle_cast/create~n"),
    Path = maps:get(folder_path, State) ++ atom_to_list(File_name),
    fiser_save_maybe(Path, File_contents, maps:get(inactive, State)),
    NextId = maps:get(next_id, State),
    fiser_forward_create(NextId, File_name, File_contents, R),
    {noreply, State};

fiser_handle_command({inactive, _}, State) ->
    write_log("fiser:handle_cast/inactive~n"),
    {noreply, State#{inactive := true}};

fiser_handle_command(quit, State) ->
    write_log("fiser:handle_cast/quit~n"),
    fiser_terminate("manual quit", State).

fiser_terminate(Reason, State) ->
    {stop, Reason, State}.
    
dir_init({Num_file_servers, R}) ->
    write_log("dir_service:init~n"),
    register(dir_pid, self()),

    dir_spawn_file_servers(Num_file_servers, Num_file_servers, util:isDistributed()),
    write_log("dir_service ready for commands"),
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
    erlang:spawn_link(Node, main, fiser_init, [NameAtom, N, Total_File_Servers]),
    write_log("dir_service:spawn_file_server/" ++ atom_to_list(NameAtom)),
    dir_spawn_file_servers(N - 1, Total_File_Servers, true);
dir_spawn_file_servers(N, Total_File_Servers, false) ->
    Name = "fs" ++ integer_to_list(N - 1),
    NameAtom = list_to_atom(Name),
    erlang:spawn_link(main, fiser_init, [NameAtom, N, Total_File_Servers]),
    write_log("dir_service:spawn_file_server/" ++ atom_to_list(NameAtom)),
    dir_spawn_file_servers(N - 1, Total_File_Servers, false).


%% Handle Erlang messages
dir_handle_command({create, File_name, File_contents}, State) ->
    write_log("dir_service:handle_info/create~n"),
    File_hash = util:hashFileName(File_name, maps:get(num_servers, State)),
    PidName = "fs" ++ integer_to_list(File_hash),
    write_log("pidname: " ++ PidName),
    send({create, {File_name, File_contents, maps:get(r, State)}}, PidName, util:isDistributed()),
    {noreply, State};

dir_handle_command({inactive, Fs}, State) ->
    File_server = atom_to_list(Fs),
    write_log("dir_service:handle_info/inactive~n"),
    send({inactive, {}}, File_server, util:isDistributed()),
    {noreply, State};

dir_handle_command({quit, _}, State) ->
    write_log("dir_service:handle_info/quit~n"),
    Num_file_servers = maps:get(num_servers, State),
    dir_propogate_kill(Num_file_servers, State),
    {noreply, State};

dir_handle_command({request, File_name}, State) ->
    write_log("dir_service:handle_info/request~n"),
    File_hash = util:hashFileName(File_name, maps:get(num_servers, State)),
    Name = "fs" ++ integer_to_list(File_hash),
    send({request, {File_name, maps:get(r, State)}}, Name, util:isDistributed()),
    {noreply, State}.

dir_terminate(_Reason, State) ->
    {stop, normal, State}.

dir_propogate_kill(0, State) ->
    dir_terminate("Manual quit", State);
dir_propogate_kill(N, State) ->
    PidName = list_to_atom("fs" ++ integer_to_list(N)),
    send(quit, PidName, util:isDistributed()),
    dir_propogate_kill(N-1, State).
    



write_log(_Message) ->

    ok.
send(Msg, "dir_pid", true) ->
	write_log("Attempting to send distributed: dir_pid"),
	{dir_pid, ds@localhost} ! Msg;
send(Msg, Name, true) ->
	write_log("Attempting to send distributed: " ++ Name),
	{list_to_atom(Name), list_to_atom(Name ++ "@localhost")} ! Msg;
send(Msg, Name, false) ->
	write_log("Attempting to send concurrent: " ++ Name),
	whereis(list_to_atom(Name)) ! Msg.