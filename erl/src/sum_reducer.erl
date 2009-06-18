-module(sum_reducer).
-export([start_fn/0,reduce/2]).
%-compile(export_all).

start_fn() ->
    NewWorkerFn = 
	fun(InFile,OutFile) ->
		spawn(?MODULE,reduce,[InFile,OutFile])
	end,
    NewWorkerFn.

reduce(InputFile,OutputFile) ->
    KVList = file_util:read(InputFile),
    Result = [ {lists:sum(Values),K} || {K,Values} <- KVList ],
    ResultFiltered = lists:filter(fun({Sum,_Pair}) -> Sum > 5 end, Result),
    file_util:write(OutputFile,ResultFiltered),
    receive
	{ack,Pid} ->
	    Pid ! {ack,self()}
    end.


