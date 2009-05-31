-module(sketch_to_id).
-compile(export_all).
-include("debug.hrl").

start(Receiver) ->
    spawn(?MODULE,init,[Receiver]).

init(Receiver) ->
    put(receiver,Receiver),
    loop(dict:new()).

loop(Store) ->
%    d(">loop ~p\n",[process_info(self(),message_queue_len)]),
    receive
	{ack,Pid} ->
	    Pid ! {ack,self()},
	    loop(Store);

	{Id, {sketch,Sketch}} ->
%	    d("storing Id ~w Ske ~w\n",[Id,Sketch]),
	    loop(add_to_store(Id,Sketch,Store));
    
	M ->
	    d("unexpected ~p\n",[M]),
	    loop(Store)

    end.

add_to_store(Id,Sketch,Store) -> 
    case dict:is_key(Sketch,Store) of
	true ->
	    Set = dict:fetch(Sketch,Store),
	    case ordsets:is_element(Id,Set) of
		true -> % already an element, ignore
		    Store;
		_ ->
		    emit_new_combos(Id,Set),
		    dict:store(Sketch,ordsets:add_element(Id,Set),Store)
	    end;
	_ ->
	    dict:store(Sketch,
		       ordsets:add_element(Id,ordsets:new()),
		       Store)
    end.

emit_new_combos(Id1,Set) ->
    lists:foreach(
      fun(Id2) -> get(receiver) ! { sketch_in_common, Id1, Id2} end,
      Set
      ).

