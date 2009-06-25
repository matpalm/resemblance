-module(sort_collate).
-export([start_fn/0,read_sort_collate/2]).
-include("debug.hrl").

start_fn() ->
    NewWorkerFn = 
	fun(InFile,OutFile) ->
		spawn(?MODULE,read_sort_collate,[InFile,OutFile])
	end,
    NewWorkerFn.

read_sort_collate(InFile,OutFile) ->
    KVList = bin_parser:read_file_as_list(InFile),
    Collated = collect_values_for_key(lists:sort(KVList)),
    %Filtered = filter(Collated),
    file_util:write(OutFile, Collated),
    map_reduce:worker_done().

% assume input sorted ??
% in  [ {k1,v1}, {k1,v2}, {k2,v1} ]
% out [ {k1,[v1,v2]}, {k2,[v1]} ]
collect_values_for_key([]) ->
    [];

collect_values_for_key(KVList) ->
    {K1,_V1} = hd(KVList),
    collect_values_for_key(KVList,K1,[],[]).

collect_values_for_key([],CombiningKey,Values,Result) ->
    FinalResult = [{CombiningKey,Values}|Result],
    lists:reverse(FinalResult);

collect_values_for_key([{K,V}|RemainingKV],CombiningKey,Values,Result) ->
    case K == CombiningKey of
	true ->
	    collect_values_for_key(RemainingKV,K,[V|Values],Result);
	false ->
	    collect_values_for_key(RemainingKV,K,[V],[{CombiningKey,Values}|Result])
    end.

     
    

