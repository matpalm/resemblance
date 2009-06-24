-module(sum_reducer).
-export([start_fn/0,reduce/2]).

start_fn() ->
    NewWorkerFn = 
	fun(InFile,OutFile) ->
		spawn(?MODULE,reduce,[InFile,OutFile])
	end,
    NewWorkerFn.

reduce(InputFile,OutputFile) ->
    KVList = file_util:read(InputFile),
    Result = [ {lists:sum(Values),K} || {K,Values} <- KVList ],
    ResultFiltered = filter_if_required(Result),
    file_util:write(OutputFile,ResultFiltered),
    map_reduce:worker_done().

filter_if_required(List) ->
    case opts:int_prop(min_sum, -1) of
	-1 -> List; % no filter
	N  -> lists:filter(fun({Sum,_Pair}) -> Sum > N end, List)
    end.
	     


