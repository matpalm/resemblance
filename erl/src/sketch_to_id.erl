-module(sketch_to_id).
-export([start/2,init/1,routing_fn/1]).
-include("debug.hrl").

start(Node,ReceiverFn) ->
    spawn(Node,?MODULE,init,[ReceiverFn]).

init(ReceiverFn) ->
    put(receiver,ReceiverFn),
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
 
	dump ->
	    lists:foreach(
	      fun({K,V}) -> d("K=~p V=~w\n",[K,length(sets:to_list(V))]) end,
	      dict:to_list(Store)
	      );

	M ->
	    d("unexpected ~p\n",[M]),
	    loop(Store)

    end.

add_to_store(Id,Sketch,Store) -> 
    case dict:is_key(Sketch,Store) of
	true ->
	    Set = dict:fetch(Sketch,Store),
	    case sets:is_element(Id,Set) of
		true -> % already an element, ignore
		    Store;
		_ ->
%		    d("emit new combo Sketch=~p Id=~p Others=~p\n",[Sketch,Id,length(sets:to_list(Set))]),
		    emit_new_combos(Id,Set),
		    dict:store(Sketch,sets:add_element(Id,Set),Store)
	    end;
	_ ->
	    dict:store(Sketch,
		       sets:add_element(Id,sets:new()),
		       Store)
    end.

emit_new_combos(Id1,Set) ->
    F = get(receiver),
    sets:fold(
      fun(Id2,_) -> F({ sketch_in_common, Id1, Id2 }), nil end,
      nil,
      Set
      ).

routing_fn(Pids) ->
    fun({_,{sketch,Sketch}}=Msg) -> 
	    Idx = Sketch rem length(Pids),
%	    d("route ~w to ~w\n",[Msg,Idx]),
	    lists:nth(Idx+1, Pids) ! Msg
    end.
