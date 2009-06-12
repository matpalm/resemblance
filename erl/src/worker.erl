-module(worker).
-export([start/1, init/2]).
-include("debug.hrl").
-define(DUMP_FREQ, 50).

start(Id) ->
    HashSeed = util:uhash_seed(opts:shingle_size()),
    spawn(?MODULE,init,[Id,HashSeed]).

init(Id,HashSeed) ->
    put(id,Id),
    put(num_flushes,0),
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
	    SketchesInCommon3 = write_to_disk_if_enough_entries(SketchesInCommon2),
	    loop({HashSeed,SketchToId2,SketchesInCommon3}); 

	dump ->
	    sketches:write(next_filename(),SketchesInCommon),
	    loop(State);
	    
	M ->
	    d("unexpected ~p\n",[M]),
	    loop(State)

%    after 5000 ->
%	    d("timeout")
    end.

shingles_to_sketch(Seed, Shingles) ->
    Hashes = [ util:uhash(S,Seed) || S <- Shingles ],
    lists:min(Hashes).

add_to_store(Id,Sketch,SketchToId) -> 
    case dict:is_key(Sketch,SketchToId) of
	true ->
	    Set = dict:fetch(Sketch,SketchToId),
	    case sets:is_element(Id,Set) of
		true -> % already an element, ignore
		    {nil, SketchToId};
		_ ->
%		    d("emit new combo Sketch=~p Id=~p Others=~p\n",[Sketch,Id,length(sets:to_list(Set))]),
		    %emit_new_combos(Id,Set),
		    {{Id,Set}, dict:store(Sketch,sets:add_element(Id,Set),SketchToId)}
	    end;
	_ ->
	    {nil, dict:store(Sketch,
		       sets:add_element(Id,sets:new()),
		       SketchToId)}
    end.

update_sketches_in_common(nil, SketchesInCommon) ->
    SketchesInCommon;

update_sketches_in_common({Id1,Set}, SketchesInCommon) ->
    NewCombos = [ { Id2, Id1 } || Id2 <- sets:to_list(Set) ],
    NewCombos ++ SketchesInCommon.

write_to_disk_if_enough_entries(List) when length(List) < ?DUMP_FREQ ->
    List;

write_to_disk_if_enough_entries(List) ->
    Filename = next_filename(),
    spawn(sketches,write,[Filename,List]),
    [].
    
next_filename() ->
    NumFlushes = get(num_flushes),
    put(num_flushes, NumFlushes+1),
    integer_to_list(get(id)) ++ "." ++ integer_to_list(NumFlushes).
    
