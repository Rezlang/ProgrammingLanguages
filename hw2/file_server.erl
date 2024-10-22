-module(file_server).
-behaviour(gen_server).


%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Starts the gen_server
start_link(Args) ->
    gen_server:start_link({local, file_server}, file_server, Args, []).


%% gen_server callback functions
init({Name}) ->
    Folder_path = "./" ++ Name ++ "/",
    {ok, #{inactive => false, next_pid => null, folder_path => Folder_path}}.

forward_request(_,  File_name, R, Client_pid) when R < 1->
    Client_pid ! File_name ++ ".err",
    ok;
forward_request(NextPid, File_name, R, Client_pid) ->
    NextPid ! {request, {File_name, R-1, Client_pid}},
    ok.


handle_call({request, {File_name, R, Client_pid}}, _From, State) ->
    try
        Path = maps:get(folder_path, State),
        Response = util:readFile(Path ++ File_name),
        Client_pid ! {request_response, Response}
    catch
        error:badmatch -> 
            forward_request(maps:get(next_pid, State), File_name, R, Client_pid)
    end,
    {noreply, State}.

forward_create(_, _, _, R) when R < 1 ->
    ok;
forward_create(NextPid, File_name, File_contents, R) ->
    NextPid ! {create, File_name, File_contents, R-1},
    ok.
save_maybe(_, _, true) ->
    ok;
save_maybe(Path, File_contents, false) ->
        
    util:saveFile(Path, File_contents).

handle_cast({create, {File_name, File_contents, R}}, State) ->
    Path = maps:get(folder_path, State) ++ File_name,
    save_maybe(Path, File_contents, maps:get(inactive, State)),
    NextPid = maps:get(next_pid, State),
    ok = forward_create(NextPid, File_name, File_contents, R),
    {noreply, State};

handle_cast({inactive, _}, State) ->
    {noreply, State#{inactive := true}};

handle_cast({next_pid, Val}, State) ->
    {noreply, State#{next_pid := Val}};

handle_cast(quit, State) ->
    terminate("manual quit", State).

terminate(_Reason, _State) ->
    ok.