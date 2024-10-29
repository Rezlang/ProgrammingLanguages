-module(fiser).

-export([init/3, terminate/2]).

init(Name, N, Total_File_Servers) ->
    util:write_log("fiser:init"),
    register(Name, self()),
    IdNum = N rem Total_File_Servers,
    Id = "fs" ++ integer_to_list(IdNum),
    Folder_path = "./servers/" ++ atom_to_list(Name) ++ "/",
    util:write_log("fiser: " ++ atom_to_list(Name) ++ " ready for commands, replicating to: " ++ Id),
    command_loop(#{inactive => false, next_id => Id, folder_path => Folder_path}).

command_loop(S) ->
    receive
        Arg -> 
            {noreply, State} = handle_command(Arg, S),
            command_loop(State)
    end.

forward_request(_,  File_name, R) when R =< 1->
    util:send({error, atom_to_list(File_name) ++ ".err"}, "client", util:isDistributed());
forward_request(NextId, File_name, R) ->
    util:send({request, {File_name, R - 1}}, NextId, util:isDistributed()).

forward_create(_, _, _, R) when R =< 1 ->
    ok;
forward_create(NextId, File_name, File_contents, R) ->
    util:send({create, {File_name, File_contents, R - 1}}, NextId, util:isDistributed()).
save_maybe(_, _, true) ->
    ok;
save_maybe(Path, File_contents, false) ->  
    util:write_log("fiser:save_maybe/~n"),
    util:write_log(Path),
    util:write_log("~n"),
    
    util:saveFile("output/" ++ Path, File_contents).

handle_command({request, {File_name, R}}, State) ->
    util:write_log("fiser:handle_call/request~n"),
    case maps:get(inactive, State) of
        true -> 
            NextId = maps:get(next_id, State),
            forward_request(NextId, File_name, R),
            {noreply, State};

        false -> 
            try
                Path = maps:get(folder_path, State),
                Response = util:readFile("output/" ++ Path ++ File_name),
                util:send({request_response, Response}, "client", util:isDistributed())
            catch
                error:badmatch -> 
                    forward_request(maps:get(next_id, State), File_name, R)
            end,
            {noreply, State}
    end;


handle_command({create, {File_name, File_contents, R}}, State) ->
    util:write_log("fiser:handle_cast/create~n"),
    Path = maps:get(folder_path, State) ++ atom_to_list(File_name),
    save_maybe(Path, File_contents, maps:get(inactive, State)),
    NextId = maps:get(next_id, State),
    forward_create(NextId, File_name, File_contents, R),
    {noreply, State};

handle_command({inactive, _}, State) ->
    util:write_log("fiser:handle_cast/inactive~n"),
    {noreply, State#{inactive := true}};

handle_command(quit, State) ->
    util:write_log("fiser:handle_cast/quit~n"),
    terminate("manual quit", State).

terminate(Reason, State) ->
    {stop, Reason, State}.