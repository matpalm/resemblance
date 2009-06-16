-module(reduce).
-export([main/0,process_and_write_files/2]).
%-compile(export_all).
-include("debug.hrl").

main() ->
    Files = sketches:files_from_command_line_args(),
    Partitioned = partition_filenames(Files),
%    io:format("Partitiones are ~p\n",[Partitioned]),
    Self = self(),
    _ReducerPids = [ spawn(?MODULE,process_and_write_files,[Self,FileList]) || {_,FileList} <- Partitioned ],
    [ receive ack -> done end || _ <- lists:seq(1,length(Partitioned)) ],
    init:stop().    

    
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

process_and_write_files(Pid,FileList) ->
%    d("processing ~w\n",[FileList]),
    FreqList = process_files(FileList, []),
%    d("intermediate result (size ~p)\n",[length(FreqList)]),
%    d("intermediate result (result ~w)\n",[FreqList]),
%    d("intermediate result (freq ~w) \n",[freq_table_of_counts(FreqList)]),%,dict:to_list(Freqs)]),
    PrunedFreqList = prune_entries(FreqList),
    lists:foreach(
      fun({{Id1,Id2},_F}) -> io:format("~p ~p\n",[Id1,Id2]) end,
      PrunedFreqList
     ),
%    sketches:write("sics_reduce/"++Id, PrunedFreqList),
%    d("final result (size ~p)\n",[length(PrunedFreqList)]),
%    d("final result ~w\n",[PrunedFreqList]),
    Pid ! ack,
    done.
    
process_files([], Result) ->
    Result;

process_files([File|FileList], FreqList) ->    
    SketchPairs = sketches:read(File),
%    d("~p has ~p entries\n",[File,length(SketchPairs)]),
    %d("freqlist ~w\n",[FreqList]),
    Combined = combine(SketchPairs, FreqList),
    %d("combined ~w\n",[Combined]),
    process_files(FileList,Combined).

% FreqList = [ {{1,2},2}, {{3,4},1}, ... ]
combine(FL1, FL2) ->
    case length(FL1) < length(FL2) of 
	true  -> combine(FL1, FL2, []);
	false -> combine(FL2, FL1, [])
    end.

combine([],[],Acc) ->
    lists:reverse(Acc);

combine(FL1,[],Acc) ->
    combine([],FL1,Acc);

combine([],[Freq|Freqs],Acc) ->
    combine([],Freqs,[Freq|Acc]);

combine([{SP1,Freq1}=F1|Freqs1]=L1,
        [{SP2,Freq2}=F2|Freqs2]=L2,
        Acc) ->
    case SP1 == SP2 of
	true -> combine(Freqs1,Freqs2,[{SP1,Freq1+Freq2}|Acc]);
	false -> case SP1 < SP2 of
		     true  -> combine(Freqs1,L2,[F1|Acc]);
		     false -> combine(L1,Freqs2,[F2|Acc])		 
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
	       
