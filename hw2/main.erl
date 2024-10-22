-module(main).

% main functions
-export([start_dir_service/2, get/1, create/1, deactivate/1, quit/0]).

% can access own ual w/ node()
% can acces own PID w/ self()

% you are free (and encouraged) to create helper functions
% but note that the functions that will be called when
% grading will be those below

% when starting the Directory Service and File Servers, you will need
% to register a process name and spawn a process in another node

% starts a directory service and file servers N and parameter R
start_dir_service(N, R) ->
	pass.
	% CODE THIS

% requests file information from the Directory Service on File
% then requests the file from the location retrieved from Directory Service
get(File) ->
	pass.
	% CODE THIS

% gives Directory Service the name of File to create
create(File) ->
	pass.
	% CODE THIS

% deactivates a server, argument is a non qualified node id
deactivate(FsID) ->
	pass.
	% CODE THIS

% sends shutdown message to ds@localhost
quit() ->
	pass.
	% CODE THIS