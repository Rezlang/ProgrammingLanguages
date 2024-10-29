% Stores functions to be used by students
-module(util).
-export([isDistributed/0,hashFileName/2,createDir/1,readFile/1,get_all_lines/1,saveFile/2,write_log/1, send/3]).

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
	io:format(File, "~s~n", [pid_to_list(self()) ++ ": " ++ Message]),
	
	% Close the file
	file:close(File),
	
	ok.
send(Msg, "dir_pid", true) ->
	write_log("Attempting to send distributed: dir_pid"),
	{dir_pid, ds@localhost} ! Msg;
send(Msg, Name, true) ->
	write_log("Attempting to send distributed: " ++ Name),
	{list_to_atom(Name), list_to_atom(Name ++ "@localhost")} ! Msg;
send(Msg, Name, false) ->
	write_log("Attempting to send concurrent: " ++ Name),
	whereis(Name) ! Msg.
