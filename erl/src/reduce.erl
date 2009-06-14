-module(reduce).
-export([main/0,process_and_write_files/2,display_result/0]).
%-compile(export_all).
-include("debug.hrl").

main() ->
    Files = files_from_command_line_args(),
    Partitioned = partition_filenames(Files),
    io:format("Partitiones are ~p\n",[Partitioned]),
    _ReducerPids = [ spawn(?MODULE,process_and_write_files,[Id,FileList]) || {Id,FileList} <- Partitioned ],
    done.    

files_from_command_line_args() ->
    {ok,Args} = init:get_argument(files),
    hd(Args).
    

% given  ["1.0.gz","1.1.gz","2.0.gz","2.1.gz","2.2.gz"] 
% return [ {"0",["1.0.gz","2.0.gz"]} , {"1",["1.1.gz","2.1.gz"]}, {2,["2.2.gz"]} 
% recall: N.M is the Mth emit set of sketcher N.
% partition strategy rationale: 
% - no sketches in common for any given N.M 
% - no instances of N.* with have an overlap of sketches in common
% - most likely case of overlap will be N1.M and N2.M, though this will diverge as M increases

partition_filenames(Filenames) ->
    FilesSplit = [ split(Filename) || Filename <- Filenames ],
    lists:keysort(1,partition(FilesSplit)).
    
split(Filename) ->
    Bits = re:split(Filename,"\\."),
    [ binary_to_list(S) || S <- Bits ].

partition(Files) ->
    partition(Files, dict:new()).

partition([],Partitions) ->
    dict:to_list(Partitions);

partition([[_,B,_]=File|Files],Partitions) ->
    [A,B,C] = File,
    Filename = A ++ "." ++ B ++ "." ++ C,
    case dict:is_key(B,Partitions) of
	true ->  partition(Files,dict:append(B,Filename,Partitions));
	false -> partition(Files,dict:store(B,[Filename],Partitions))
    end.

process_and_write_files(Id,FileList) ->
%    d("processing ~w\n",[FileList]),
    FreqList = process_files(FileList, []),
%    d("intermediate result (size ~p)\n",[length(FreqList)]),
%    d("intermediate result (result ~w)\n",[FreqList]),
%    d("intermediate result (freq ~w) \n",[freq_table_of_counts(FreqList)]),%,dict:to_list(Freqs)]),
    PrunedFreqList = prune_entries(FreqList),
    sketches:write("sics_reduce/"++Id, PrunedFreqList),
%    d("final result (size ~p)\n",[length(PrunedFreqList)]),
%    d("final result ~w\n",[PrunedFreqList]),
    done.
    
process_files([], Result) ->
    Result;

process_files([File|FileList], FreqList) ->    
    SketchPairs = sketches:read(File),
    d("~p has ~p entries\n",[File,length(SketchPairs)]),
    %d("freqlist ~w\n",[FreqList]),
    Combined = combine(SketchPairs, FreqList),
    %d("combined ~w\n",[Combined]),
%    Freq2 = lists:foldl(
%      fun(SketchPair,Acc) -> dict:update_counter(SketchPair,1,Acc) end,
%      FreqList,
%      SketchPairs
%     ),
    process_files(FileList,Combined).

% SketchPairs = [ {1,2}, {3,4}, ... ]
% FreqList = [ {{1,2},2}, {{3,4},1}, ... ]
combine(SketchPairs, FreqList) ->
    combine(SketchPairs, FreqList, []).

combine([],[],Acc) ->
    lists:reverse(Acc);

combine([SketchPair|RemainingSketchPairs],[],Acc) ->
    combine(RemainingSketchPairs,[],[{SketchPair,1}|Acc]);

combine([],[Freq|RemainingFreqs],Acc) ->
    combine([],RemainingFreqs,[Freq|Acc]);

combine([SP1|RemainingSketchPairs]=L1,[{SP2,Freq}|RemainingFreqs]=L2,Acc) ->
    case SP1 == SP2 of
	true -> combine(RemainingSketchPairs,RemainingFreqs,[{SP1,Freq+1}|Acc]);
	false -> case SP1 < SP2 of
		     true  -> combine(RemainingSketchPairs,L2,[{SP1,1}|Acc]);
		     false -> combine(L1,RemainingFreqs,[{SP2,Freq}|Acc])		 
		 end
    end.    

prune_entries(FreqList) ->
    Cutoff = opts:sketches_in_common_cutoff(),
    lists:filter(
      fun({_,F}) -> F > Cutoff end,
      FreqList
     ).

% K -> V, sketch_pair -> freq, eg {1,2} -> 12
freq_table_of_counts(FreqList) ->
    Counts = [ V || {_K,V} <- FreqList],
    Freqs = lists:foldl(
      fun(C,Freqs) -> dict:update_counter(C,1,Freqs) end,
      dict:new(),
      Counts),
    lists:keysort(1,dict:to_list(Freqs)).
	       
display_result() ->
    {ok,F} = init:get_argument(file),
    F2 = hd(hd(F)),    
    d("~w\n",[lists:keysort(2,sketches:read(F2))]).
    
