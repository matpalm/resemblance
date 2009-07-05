-module(sort_collate).
-export([process/2]).
-include("debug.hrl").

process(InFile,OutFile) ->
    In = bin_parser:open_file_for_read(InFile),
    Out = bin_parser:open_file_for_write(OutFile),
    KVList = slurp(In,[]),
    Collated = collect_values_for_key(lists:sort(KVList)),
    lists:foreach(
      fun(T) -> bin_parser:write(Out, T) end, 
      Collated
     ),
    util:ack_response().    

slurp(In, Acc) ->
    Parsed = bin_parser:read(In),
    case Parsed of
	eof ->     Acc;
	{ok,KV} -> slurp(In,[KV|Acc])
    end.

% assume input sorted 
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

     
    

