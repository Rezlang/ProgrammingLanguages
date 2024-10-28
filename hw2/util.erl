% Stores functions to be used by students
-module(util).
-export([get_global_pid/2,isDistributed/0,hashFileName/2,createDir/1,readFile/1,get_all_lines/1,saveFile/2,write_log/1]).

% Functions in here can be called in main.erl by doing (for example):
% util:saveFile(path/to/file.txt, "string")

% function to check if a node is distributed or not
% use when you start implementing the distributed version
isDistributed() ->
	node() =/= nonode@nohost.

% hashes the name of a file based on the number of servers
hashFileName(FileName, NumServers) -> 
	NameString = atom_to_list(FileName),
	AsciiSum = lists:foldr(fun(Elem, Acc) -> Acc + Elem end, 0, NameString),
	Hash = AsciiSum rem NumServers,
	Hash.

% creates a directory
createDir(Location) ->
	file:make_dir(Location).

% saves a String to a file located at Location
saveFile(Location, String) ->
	file:write_file(Location, String).

% returns the contents of a file located at FileName
readFile(FileName) ->
	{ok, Device} = file:open(FileName, [read]),
	try get_all_lines(Device)
		after file:close(Device)
	end.

% Helper function for readFile
get_all_lines(Device) ->
	case io:get_line(Device, "") of
		eof  -> [];
		Line -> Line ++ get_all_lines(Device)
	end.

write_log(Message) ->
	% Define the log file path
	LogFile = "./logs/test.log",
	
	% Open the file in append mode
	{ok, File} = file:open(LogFile, [append]),
	
	% Write the message followed by a newline
	io:format(File, "~s~n", [Message]),
	
	% Close the file
	file:close(File),
	
	ok.
get_global_pid(0, Name) -> 
	util:write_log("couldnt find pid with name " ++ atom_to_list(Name)),
	ok;
get_global_pid(N, Name) ->
	util:write_log("getting " ++ atom_to_list(Name)),
	case global:whereis_name(Name) of
		Pid when is_pid(Pid) -> 
			util:write_log("Successfully got pid: " ++ pid_to_list(Pid)),
			Pid;
		_ -> 
			timer:sleep(50), % Wait and retry
			get_global_pid(N-1, Name)
	end.

