-module(sort_collate).
-export([initial_state/0, process/3, finished/2]).
-include("debug.hrl").

initial_state() ->
    [].

process(Term, Acc, _EmitFn) ->
    [Term|Acc].

finished(KVList, EmitFn) ->
    Collated = collect_values_for_key(lists:sort(KVList)),
    lists:foreach(
      fun(T) -> EmitFn(T) end, 
      Collated
     ).

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

     
    

