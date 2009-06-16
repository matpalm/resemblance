-module(worker).
-export([start/1, init/2, dump_sketches_in_common/2]).
-include("debug.hrl").
-define(DUMP_FREQ, 200000).

start(Id) ->
    HashSeed = util:uhash_seed(opts:shingle_size()),
    spawn(?MODULE,init,[Id,HashSeed]).

init(Id,HashSeed) ->
    put(id,Id),
%    put(num_flushes,0),
    loop({HashSeed,dict:new(),[]}).

loop({HashSeed,SketchToId,SketchesInCommon}=State) ->
    receive
	{ack,Pid} ->
	    Pid ! {ack,self()}, 
 	    loop(State);       		
	
	{Id, {shingles, Shingles}} ->
    %	    d("got shingles ~p\n",[Shingles]),
	    Sketch = shingles_to_sketch(HashSeed, Shingles),
%	    d("Id=~p sketch=~p\n",[Id,Sketch]),
	    {NewCombo, SketchToId2} = add_to_store(Id,Sketch,SketchToId), 
	    SketchesInCommon2 = update_sketches_in_common(NewCombo,SketchesInCommon),
	    loop({HashSeed,SketchToId2,SketchesInCommon2}); 

	{dump,N} ->
	    Filename = next_filename(N),
	    spawn(?MODULE, dump_sketches_in_common ,[Filename,SketchesInCommon]),
	    loop({HashSeed,SketchToId,[]});
	
	{final_dump,N} ->
	    Filename = next_filename(N),
	    worker:dump_sketches_in_common(Filename,SketchesInCommon),
	    loop({HashSeed,SketchToId,[]});

	M ->
	    d("unexpected ~p\n",[M]),
	    loop(State)

    end.

shingles_to_sketch(Seed, Shingles) ->
    Hashes = [ util:uhash(S,Seed) || S <- Shingles ],
    lists:min(Hashes).

add_to_store(Id,Sketch,SketchToId) -> 
    case dict:is_key(Sketch,SketchToId) of
	true ->
	    Set = dict:fetch(Sketch,SketchToId),
	    case ordsets:is_element(Id,Set) of
		true -> % already an element, ignore
		    {nil, SketchToId};
		_ ->
%		    d("emit new combo Sketch=~p Id=~p Others=~p\n",[Sketch,Id,length(sets:to_list(Set))]),
		    %emit_new_combos(Id,Set),
		    {{Id,Set}, dict:store(Sketch,ordsets:add_element(Id,Set),SketchToId)}
	    end;
	_ ->
	    {nil, dict:store(Sketch,
		       ordsets:add_element(Id,ordsets:new()),
		       SketchToId)}
    end.

update_sketches_in_common(nil, SketchesInCommon) ->
    SketchesInCommon;

update_sketches_in_common({Id1,Set}, SketchesInCommon) ->
    NewCombos = [ { Id2, Id1 } || Id2 <- ordsets:to_list(Set) ],
    lists:merge(NewCombos,SketchesInCommon).

%write_to_disk_if_enough_entries(List) when length(List) < ?DUMP_FREQ ->
%    List;
%
%write_to_disk_if_enough_entries(List) ->
%    Filename = next_filename(),
%    spawn(sketches,write,[Filename,List]),
%    [].

dump_sketches_in_common(Filename,SketchesInCommon) ->
    SketchesInCommonFreq = [ {S,1} || S <- SketchesInCommon ],
    sketches:write(Filename,SketchesInCommonFreq).
    
next_filename(N) ->
    %NumFlushes = get(num_flushes),
    %put(num_flushes, NumFlushes+1),
    "sics_map/" ++ integer_to_list(get(id)) ++ "." ++ integer_to_list(N).
    
