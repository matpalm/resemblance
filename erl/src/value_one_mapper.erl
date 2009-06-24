-module(value_one_mapper).
-export([start_fn/0,map/2]).
-include("debug.hrl").

start_fn() ->
    NewWorkerFn = 
	fun(InFile,OutFile) ->
		spawn(?MODULE,map,[InFile,OutFile])
	end,
    NewWorkerFn.

map(InputFile,OutputFile) ->
    Input = file_util:read(InputFile),
    Result = [ {Data,1} || Data <- Input ],
    file_util:write(OutputFile,Result),
    map_reduce:worker_done().




    
