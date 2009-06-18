-module(sketch_mapper).
-export([start_fn/0,map/3]).
-include("debug.hrl").

start_fn() ->
    SS = opts:sketch_size(),
    Seeds = [ util:uhash_seed(SS) || _ <- lists:seq(1,SS) ],    
    NewWorkerFn = 
	fun(InFile,OutFile) ->
		spawn(?MODULE,map,[InFile,OutFile,Seeds])
	end,
    NewWorkerFn.

map(InputFile,OutputFile,Seeds) ->
    Lines = file_util:read(InputFile),
    Result = process(Lines,Seeds),
    file_util:write(OutputFile,Result),
    receive
	{ack,Pid} ->
	    Pid ! {ack,self()}
    end.

process(Lines,Seeds) ->
    process(Lines,Seeds,[]).

process([],_Seeds,Acc) ->
    Acc;

process([{Id,Content}|Lines],Seeds,Acc) ->
    Shingles = util:shingles(Content),
    Sketches = shingles_to_sketches(Seeds, Shingles), % TODO: is this sort required? shuffler now sorts...
    SketchesId = [ {Sketch,Id} || Sketch <- Sketches],
    Acc2 = lists:merge(Acc,SketchesId),
    process(Lines,Seeds,Acc2).

shingles_to_sketches(Seeds, Shingles) ->
    shingles_to_sketches(Seeds, Shingles, []).

shingles_to_sketches([], _Shingles, Sketches) ->
    Sketches;

shingles_to_sketches([Seed|Seeds], Shingles, Sketches) ->
    Sketch = shingles_to_sketch(Seed, Shingles),
    shingles_to_sketches(Seeds, Shingles, [Sketch|Sketches]).

shingles_to_sketch(Seed, Shingles) ->
    Hashes = [ util:uhash(S,Seed) || S <- Shingles ],
    lists:min(Hashes).

%add_to_store(Id,Sketch,SketchToId) -> 
%    case dict:is_key(Sketch,SketchToId) of
%	true ->
%	    Set = dict:fetch(Sketch,SketchToId),
%	    case ordsets:is_element(Id,Set) of
%		true -> % already an element, ignore
%		    {nil, SketchToId};
%		_ ->
%%		    d("emit new combo Sketch=~p Id=~p Others=~p\n",[Sketch,Id,length(sets:to_list(Set))]),
%		    %emit_new_combos(Id,Set),
%		    {{Id,Set}, dict:store(Sketch,ordsets:add_element(Id,Set),SketchToId)}
%	    end;
%	_ ->
%	    {nil, dict:store(Sketch,
%		       ordsets:add_element(Id,ordsets:new()),
%		       SketchToId)}
%    end.

%update_sketches_in_common(nil, SketchesInCommon) ->
%    SketchesInCommon;

%update_sketches_in_common({Id1,Set}, SketchesInCommon) ->
%    NewCombos = [ { Id2, Id1 } || Id2 <- ordsets:to_list(Set) ],
%    lists:merge(NewCombos,SketchesInCommon).

%write_to_disk_if_enough_entries(List) when length(List) < ?DUMP_FREQ ->
%    List;
%
%write_to_disk_if_enough_entries(List) ->
%    Filename = next_filename(),
%    spawn(sketches,write,[Filename,List]),
%    [].
    
next_filename(N) ->
    file_util:output_dir() ++ "/" ++ 
	integer_to_list(get(worker_id)) ++ "." ++ 
	integer_to_list(N).
    
