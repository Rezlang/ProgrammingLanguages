-module(main).

-export([start_dir_service/2, get/1, create/1, deactivate/1, quit/0]).

start_dir_service(N, R) ->
    util:write_log("start_dir_service"),
    dir_service:init({N, R}),
    client_register().

create(Name) ->
    util:write_log("client:create"),
    client_register(),
    Contents = util:readFile("./input/" ++ Name),
    util:send({create, Name, Contents}, "dir_pid", util:isDistributed()).


get(Name) ->
    util:write_log("client:get"),
    client_register(),
    
    util:send({request, Name}, "dir_pid", util:isDistributed()),
    receive
        {request_response, File_contents} ->
            util:saveFile("./output/downloads/" ++ Name, File_contents),
            util:write_log("Received file contents~n"),
            ok;
        {error, Error_file} ->
            util:saveFile("./output/downloads/" ++ Error_file, ""),
            util:write_log("Received error file~n"),
            error
    end.

deactivate(Name) ->
    util:write_log("client:deactivate"),
    util:send({inactive, Name}, "dir_pid", util:isDistributed()).

quit() ->
    util:write_log("client:quit"),
    util:send({quit, nothing}, "dir_pid", util:isDistributed()),
    erlang:halt().

client_register() ->
    try
        register(client, self())
    catch
        true -> ok;
        _ ->
            unregister(client),
            register(client, self())
    end.


