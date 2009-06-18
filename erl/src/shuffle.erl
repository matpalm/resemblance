-module(shuffle).
-compile(export_all).
%-export([main/0,collect_values_for_key/1]).
-include("debug.hrl").

main() ->
    InputFiles = file_util:input_files(),
    SortedKeyValues = process(InputFiles,[]),
    Filtered = filter(SortedKeyValues),
    util:ensure_output_dir_created(),
    util:distribute_over_N_files(Filtered,10),
    init:stop().

process([],Result) ->
    Result;

process([File|Files],Result) ->
    Data = file_util:read(File),
    CollectedFile = collect_values_for_key(lists:sort(Data)),
    CombinedWithResult = util:merge(CollectedFile,Result),
%    d("file=~p collected #~w combined #~w\n",[File,length(CollectedFile),length(CombinedWithResult)]),
    process(Files,CombinedWithResult).
    
filter(KeyValues) ->
    lists:filter(
      fun({_K,V}) -> length(V) > 1 end,
      KeyValues
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
    

