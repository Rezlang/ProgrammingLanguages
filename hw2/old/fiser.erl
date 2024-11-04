-module(fiser).

-export([fiser_init/3, fiser_terminate/2]).

fiser_init(Name, N, Total_File_Servers) ->
    util:write_log("fiser:init"),
    register(Name, self()),
    IdNum = N rem Total_File_Servers,
    Id = "fs" ++ integer_to_list(IdNum),
    Folder_path = "./servers/" ++ atom_to_list(Name) ++ "/",
    util:write_log("fiser: " ++ atom_to_list(Name) ++ " ready for commands, replicating to: " ++ Id),
    fiser_command_loop(#{inactive => false, next_id => Id, folder_path => Folder_path}).

fiser_command_loop(S) ->
    receive
        Arg -> 
            {noreply, State} = fiser_handle_command(Arg, S),
            fiser_command_loop(State)
    end.

fiser_forward_request(_,  File_name, R) when R =< 1->
    util:send({error, atom_to_list(File_name) ++ ".err"}, "client", util:isDistributed());
fiser_forward_request(NextId, File_name, R) ->
    util:send({request, {File_name, R - 1}}, NextId, util:isDistributed()).

fiser_forward_create(_, _, _, R) when R =< 1 ->
    ok;
fiser_forward_create(NextId, File_name, File_contents, R) ->
    util:send({create, {File_name, File_contents, R - 1}}, NextId, util:isDistributed()).
fiser_save_maybe(_, _, true) ->
    ok;
fiser_save_maybe(Path, File_contents, false) ->  
    util:write_log("fiser:save_maybe/~n"),
    util:write_log(Path),
    util:write_log("~n"),
    
    util:saveFile("output/" ++ Path, File_contents).

fiser_handle_command({request, {File_name, R}}, State) ->
    util:write_log("fiser:handle_call/request~n"),
    case maps:get(inactive, State) of
        true -> 
            NextId = maps:get(next_id, State),
            fiser_forward_request(NextId, File_name, R),
            {noreply, State};

        false -> 
            try
                Path = maps:get(folder_path, State),
                Response = util:readFile("output/" ++ Path ++ File_name),
                util:send({request_response, Response}, "client", util:isDistributed())
            catch
                error:badmatch -> 
                    fiser_forward_request(maps:get(next_id, State), File_name, R)
            end,
            {noreply, State}
    end;


fiser_handle_command({create, {File_name, File_contents, R}}, State) ->
    util:write_log("fiser:handle_cast/create~n"),
    Path = maps:get(folder_path, State) ++ atom_to_list(File_name),
    fiser_save_maybe(Path, File_contents, maps:get(inactive, State)),
    NextId = maps:get(next_id, State),
    fiser_forward_create(NextId, File_name, File_contents, R),
    {noreply, State};

fiser_handle_command({inactive, _}, State) ->
    util:write_log("fiser:handle_cast/inactive~n"),
    {noreply, State#{inactive := true}};

fiser_handle_command(quit, State) ->
    util:write_log("fiser:handle_cast/quit~n"),
    fiser_terminate("manual quit", State).

fiser_terminate(Reason, State) ->
    {stop, Reason, State}.