-module(mapper).
-export([start/2, init/2]).
-include("debug.hrl").
-define(DUMP_FREQ, 200000).

start(Seeds,WorkerId) ->
    d("STARTING\n"),
    spawn(?MODULE,init,[Seeds,WorkerId]).

init(Seeds,WorkerId) ->
    put(worker_id, WorkerId),
    loop(Seeds,[]).

loop(HashSeeds, CollectedSketchesIds) ->
    receive
	{ack,Pid} ->
	    Pid ! {ack,self()}, 
 	    loop(HashSeeds, CollectedSketchesIds);       		
	
	{line,{Id,Line}} ->
%    	    d("got id=~p line=~p\n",[Id,Line]),
	    Shingles = util:shingles(Line),
	    Sketches = lists:sort(shingles_to_sketches(HashSeeds, Shingles)),
	    SketchesId = [ {Sketch,Id} || Sketch <- Sketches],
%	    d("sketches ~w\n",[SketchesId]),
	    CollectedSketchesIds2 = lists:merge(CollectedSketchesIds,SketchesId), 
%	    d("collected sketches ~w\n",[CollectedSketchesIds2]),
	%    Sketch = shingles_to_sketch(HashSeed, Shingles),
%	    d("Id=~p sketch=~p\n",[Id,Sketch]),
	 %   {NewCombo, SketchToId2} = add_to_store(Id,Sketch,SketchToId), 
	 %   SketchesInCommon2 = update_sketches_in_common(NewCombo,SketchesInCommon),
	    loop(HashSeeds, CollectedSketchesIds2); 

	{dump,N} ->
	    Filename = next_filename(N),
	    spawn(file_util,write,[Filename,CollectedSketchesIds]),
	    loop(HashSeeds, []);
	
%	{final_dump,N} ->
%	    Filename = next_filename(N),
%	    worker:dump_sketches_in_common(Filename,SketchesInCommon),
%	    loop({HashSeed,SketchToId,[]});

	M ->
	    d("unexpected ~p\n",[M]),
	    loop(HashSeeds, CollectedSketchesIds)

    end.

shingles_to_sketches(Seeds, Shingles) ->
    shingles_to_sketches(Seeds, Shingles, []).

shingles_to_sketches([], Shingles, Sketches) ->
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
    %NumFlushes = get(num_flushes),
    %put(num_flushes, NumFlushes+1),
    "sics_map/" ++ integer_to_list(get(worker_id)) ++ "." ++ integer_to_list(N).
    
