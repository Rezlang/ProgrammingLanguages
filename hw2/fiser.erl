-module(fiser).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start_link/1]).

%% Starts the gen_server
start_link(Name) ->
    gen_server:start_link({global, Name}, fiser, Name, []).


%% gen_server callback functions
init(Name) ->
    util:write_log("fiser:init~n"),
    global:register_name(Name, self()),
    Folder_path = "./servers/" ++ atom_to_list(Name) ++ "/",
    {ok, #{inactive => false, next_pid => null, folder_path => Folder_path}}.

forward_request(_,  File_name, R, Client_pid) when R =< 1->
    Client_pid ! {error, atom_to_list(File_name) ++ ".err"};
forward_request(NextPid, File_name, R, Client_pid) ->
    gen_server:cast(NextPid, {request, {File_name, R - 1, Client_pid}}).

forward_create(_, _, _, R) when R =< 1 ->
    ok;
forward_create(NextPid, File_name, File_contents, R) ->
    gen_server:cast(NextPid, {create, {File_name, File_contents, R - 1}}).
save_maybe(_, _, true) ->
    ok;
save_maybe(Path, File_contents, false) ->  
    util:write_log("fiser:save_maybe/~n"),
    util:write_log(Path),
    util:write_log("~n"),
    
    util:saveFile("output/" ++ Path, File_contents).

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({request, {File_name, R, Client_pid}}, State) ->
    util:write_log("fiser:handle_call/request~n"),
    case maps:get(inactive, State) of
        true -> 
            NextPid = maps:get(next_pid, State),
            forward_request(NextPid, File_name, R, Client_pid),
            {noreply, State};

        false -> 
            try
                Path = maps:get(folder_path, State),
                Response = util:readFile("output/" ++ Path ++ File_name),
                Client_pid ! {request_response, Response}
            catch
                error:badmatch -> 
                    forward_request(maps:get(next_pid, State), File_name, R, Client_pid)
            end,
            {noreply, State}
    end;


handle_cast({create, {File_name, File_contents, R}}, State) ->
    util:write_log("fiser:handle_cast/create~n"),
    Path = maps:get(folder_path, State) ++ atom_to_list(File_name),
    save_maybe(Path, File_contents, maps:get(inactive, State)),
    NextPid = maps:get(next_pid, State),
    ok = forward_create(NextPid, File_name, File_contents, R),
    {noreply, State};

handle_cast({inactive, _}, State) ->
    util:write_log("fiser:handle_cast/inactive~n"),
    {noreply, State#{inactive := true}};

handle_cast({next_pid, Name}, State) ->
    util:write_log("fiser:handle_cast/next_pid~n"),
    NextPid = util:get_global_pid(3,Name),
    util:write_log("got pid"),
    {noreply, State#{next_pid := NextPid}};

handle_cast(quit, State) ->
    util:write_log("fiser:handle_cast/quit~n"),
    terminate("manual quit", State).

terminate(Reason, State) ->
    {stop, Reason, State}.