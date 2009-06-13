-module(reduce).
-compile(export_all).
-include("debug.hrl").

main() ->
    Files = files_from_command_line_args(),
    Partitioned = partition_filenames(Files),
    %io:format("Partitiones are ~p\n",[Partitioned]),
    _ReducerPids = [ spawn(?MODULE,process_files,[FileList]) || {_Id,FileList} <- Partitioned ],
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

process_files(FileList) ->
%    d("processing ~w\n",[FileList]),
    Freqs = process_files(FileList, dict:new()),
    d("intermediate result (size ~p) \n",[dict:size(Freqs)]),%,dict:to_list(Freqs)]),
    d("intermediate result (freq ~p) \n",[freq_table_of_counts(Freqs)]),%,dict:to_list(Freqs)]),
    FreqsWithoutOnes = remove_single_entries(Freqs),
    d("result (size ~p) \n",[dict:size(FreqsWithoutOnes)]).%,dict:to_list(FreqsWithoutOnes)]).
    
process_files([], Freq) ->
    Freq;

process_files([File|FileList], Freq) ->    
    SketchPairs = sketches:read(File),
    d("~p has ~p sketches in common\n",[File,length(SketchPairs)]),
    Freq2 = lists:foldl(
      fun(SketchPair,Acc) -> dict:update_counter(SketchPair,1,Acc) end,
      Freq,
      SketchPairs
     ),
    process_files(FileList,Freq2).
    
remove_single_entries(Freqs) ->
    dict:filter(
      fun(_,F) -> F > 1 end,
      Freqs
     ).

% K -> V, sketch_pair -> freq, eg {1,2} -> 12
freq_table_of_counts(Dict) ->
    Counts = [ V || {_K,V} <- dict:to_list(Dict)],
    Freqs = lists:foldl(
      fun(C,Freqs) -> dict:update_counter(C,1,Freqs) end,
      dict:new(),
      Counts),
    lists:keysort(1,dict:to_list(Freqs)).
	       
    
    
