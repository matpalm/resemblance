-module(merge).
-export([trace/0,start/0,open_files_and_merge/2]).
-include("debug.hrl").

trace() ->
    dbg:c(?MODULE,start,[],[all]).

start() ->
    % read all input key/value files and partition by key across output files
    file_util:ensure_output_dir_created(),
    PartitionToFiles = seperate_filenames_into_partitions(),   
    Workers = start_workers(PartitionToFiles),
    util:ack(Workers),
    init:stop().

seperate_filenames_into_partitions() ->
    seperate_filenames(file_util:input_files(), dict:new()).

seperate_filenames([],Dict) ->
    dict:to_list(Dict);

seperate_filenames([File|Files],Dict) ->
    Partition = partition_of(File),
    Dict2 = create_list_if_required(Partition, Dict),
    seperate_filenames(Files, dict:append(Partition, File, Dict2)).

partition_of(File) ->
    [_,Partition,_] = re:split(File, "p(\\d+)", [{return,list}]),
    Partition.

create_list_if_required(Partition, Dict) ->
    case dict:is_key(Partition, Dict) of
	true  -> Dict;
	false -> dict:store(Partition, [], Dict)
    end.

start_workers(PartitionToFiles) ->
   start_workers(PartitionToFiles, []).

start_workers([], Pids) ->
    Pids;

start_workers([{Partition,Files}|PartitionToFiles], Pids) ->
    Pid = spawn(?MODULE, open_files_and_merge, [Partition, Files]),
    start_workers(PartitionToFiles, [Pid|Pids]).

%merge_next([]) ->
%    done;
%
%merge_next([{Partition,Files}|PartitionToFiles]) ->
%    open_files_and_merge(Partition,Files),
%    merge_next(PartitionToFiles).

open_files_and_merge(Partition,Files) ->
    InFilenames = [ file_util:input_dir()++"/"++File || File <- Files ],
    OutFilename = file_util:output_dir()++"/"++Partition++".gz",

    io:format("InFilenames ~p OutFilename ~p\n",[InFilenames, OutFilename]),

    Ins = [ bin_parser:open_file_for_read(File) || File <- InFilenames ],
    Out = bin_parser:open_file_for_write(OutFilename),

    FirstReadOfIns = [ read_from_in(In) || In <- Ins ],
    IgnoringEmptyFiles = lists:filter(fun(X) -> X /= eof end, FirstReadOfIns),
    merge(Out, IgnoringEmptyFiles),

    lists:foreach(
      fun(F) -> file:close(F) end,
      [Out] ++ Ins
     ),
    
    util:ack_response().
    
read_from_in(In) ->
    Res = bin_parser:read(In),
    case Res of 
	eof -> eof;
	{ok,{K,Vs}} -> {K,Vs,In}
    end.     

merge(_Out,[]) ->
    done;

% OutHandle, [{K,VList,Continuation,InHandle}, {K,Vlist,InHandle}]
merge(Out,Ins) ->
    MinKey = lists:min([ K || {K,_,_} <- Ins ]),
    { Ins2, MatchingKVlists } = pop_key_vlists_matching(MinKey, Ins),
    bin_parser:write(Out, { MinKey, lists:flatten(MatchingKVlists) }),
    merge(Out,Ins2).

pop_key_vlists_matching(MinKey, Ins) ->
    pop_key_vlists_matching(MinKey, Ins, [], []).

pop_key_vlists_matching(_MinKey, [], AccIns, AccVLists) ->
    { AccIns, AccVLists };

pop_key_vlists_matching(MinKey, [{K,VList,InHandle}=In|Ins], AccIns, AccVLists) ->
    case MinKey == K of
	false ->
	    pop_key_vlists_matching(MinKey, Ins, [In|AccIns], AccVLists);
	true ->
	    AccVLists2 = [ VList | AccVLists],
	    In2 = read_from_in(InHandle),
	    case In2 of
		eof -> 	    
		    pop_key_vlists_matching(MinKey, Ins, AccIns, AccVLists2);
		_ ->
		    pop_key_vlists_matching(MinKey, Ins, [In2|AccIns], AccVLists2)
	    end
    end.



    

