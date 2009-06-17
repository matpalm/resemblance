-module(shuffle).
-compile(export_all).
%-export([main/0,collect_values_for_key/1]).
-include("debug.hrl").

main() ->
    Files = file_util:files_from_command_line_args(),
    SortedKeyValues = process(Files,[]),
    Filtered = filter(SortedKeyValues),
    distribute_over_N_files(Filtered,10),
    init:stop().

process([],Result) ->
    Result;

process([File|Files],Result) ->
    CollectedFile = collect_values_for_key(file_util:read(File)),
    CombinedWithResult = util:merge(CollectedFile,Result),
    d("file=~p collected #~w combined #~w\n",[File,length(CollectedFile),length(CombinedWithResult)]),
    process(Files,CombinedWithResult).
    
filter(KeyValues) ->
    lists:filter(
      fun({_K,V}) -> length(V) > 1 end,
      KeyValues
     ).
     
distribute_over_N_files(List,N) ->
    write_to_file(0,distribute_over_N_lists(List,N)).

distribute_over_N_lists(List,N) ->
    EmptyLists = lists:duplicate(N,[]),
    distribute_over_N_lists(List,EmptyLists,[]).

distribute_over_N_lists([],ToFill,Filled) ->
    ToFill ++ Filled;

distribute_over_N_lists(List,[],Filled) ->
    distribute_over_N_lists(List,Filled,[]);

distribute_over_N_lists([H|T],[LH|LT],Filled) ->
    distribute_over_N_lists(T, LT, [[H|LH]|Filled]).

write_to_file(N,[]) ->
    done;

write_to_file(N, [H|T]) ->
    file_util:write("sics_shuffled/"++integer_to_list(N), H),
    write_to_file(N+1,T).

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
%    d("K=~p V=~p RemainingKV=~w CombiningKey=~w Value2=~w Result=~w\n",[K,V,RemainingKV,CombiningKey,Values,Result]),
    case K == CombiningKey of
	true ->
	    collect_values_for_key(RemainingKV,K,[V|Values],Result);
	false ->
	    collect_values_for_key(RemainingKV,K,[V],[{CombiningKey,Values}|Result])
    end.
    

