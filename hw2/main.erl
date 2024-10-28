-module(main).

-export([start_dir_service/2, get/1, create/1, deactivate/1, quit/0]).

start_dir_service(N, R) ->
    util:write_log("start_dir_service"),
    {ok, _} = dir_service:start_link({N, R}),
    global:register_name(client, self()).

create(Name) ->
    util:write_log("client:create"),
    register_self(client),
    Dir_pid = util:get_global_pid(3,dir_pid),
    case Dir_pid of 
        ok ->
            util:write_log("Failed to find dir service");
        _ ->
            util:write_log(pid_to_list(Dir_pid)),
            Contents = util:readFile("./input/" ++ Name),
            gen_server:cast(Dir_pid, {create, Name, Contents})
    end.


get(Name) ->
    util:write_log("client:get"),
    register_self(client),
    Dir_pid = util:get_global_pid(3,dir_pid),
    case Dir_pid of
        undefined ->
            util:write_log("Error: Dir_pid not registered globally~n"),
            {error, not_found};
        _ ->
            Self = util:get_global_pid(3,client),
            gen_server:cast(Dir_pid, {request, Name, Self}),
            receive
                {request_response, File_contents} ->
                    util:saveFile("./output/downloads/" ++ Name, File_contents),
                    util:write_log("Received file contents~n"),
                    ok;
                {error, Error_file} ->
                    util:saveFile("./output/downloads/" ++ Error_file, ""),
                    util:write_log("Received error file~n"),
                    error
            end
    end.
deactivate(Name) ->
    util:write_log("client:deactivate"),
    Dir_pid = util:get_global_pid(3,dir_pid),
    gen_server:cast(Dir_pid, {inactive, Name}).

quit() ->
    util:write_log("client:quit"),
    Dir_pid = util:get_global_pid(3,dir_pid),
    gen_server:cast(Dir_pid, {quit, nothing}),
    erlang:halt().

unregister_if_distributed(true, Name) ->
    global:unregister_name(Name);
unregister_if_distributed(false, _) -> ok.

register_self(Name) ->
    case global:register_name(Name, self()) of
        {error, {already_registered, _}} -> 
            util:write_log("tried to reregister"),
            unregister_if_distributed(util:isDistributed(), Name),
            global:register_name(Name, self()),
            util:write_log("successfully registered");
        {error, _} ->
            util:write_log("Failed to register for some other reason");
        _ -> 
            util:write_log("registered client first try"),
            ok
    end.


