-module(sketch_to_id).
-compile(export_all).
-include("debug.hrl").

start(Receiver) ->
    spawn(?MODULE,init,[Receiver]).

init(Receiver) ->
    put(receiver,Receiver),
    loop(dict:new()).

loop(Store) ->
    receive
	stop -> 
	    d("stopping"),
	    d("final store ~p\n",[dict:to_list(Store)]),
	    get(receiver) ! stop,
	    exit(0);
	
	{Id, {sketch,Sketch}} ->
	    d("storing Id ~w Ske ~w\n",[Id,Sketch]),
	    loop(add_to_store(Id,Sketch,Store))
    
    after 15000 ->
	    io:format("sketch to id timeout\n"),
	    exit(1)
    end.

add_to_store(Id,Sketch,Store) -> 
    case dict:is_key(Sketch,Store) of
	true ->
	    Set = dict:fetch(Sketch,Store),
	    case ordsets:is_element(Id,Set) of
		true -> % already an element, ignore
		    Store;
		_ ->
		    Set2 = ordsets:add_element(Id,Set),
		    emit_combos(Set2),
		    dict:store(Sketch,Set2,Store)
	    end;
	_ ->
	    dict:store(Sketch,
		       ordsets:add_element(Id,ordsets:new()),
		       Store)
    end.

emit_combos(S) ->
    get(receiver) ! { combos_for, S}.
