-module(worker).
-export([start/1, loop/1]).
-include("debug.hrl").

start(DbInsertFn) ->
    HashSeed = util:uhash_seed(opts:shingle_size()),
    db:add_to_db_pool(), % would prefer connection per worker but some problems connecting...
    spawn(?MODULE,loop,[{HashSeed,dict:new(),DbInsertFn}]).

loop({HashSeed,Store,DbInsertFn}=State) ->
    receive
	{ack,Pid} ->
	    Pid ! {ack,self()},
	    loop(State);

	{Id, {shingles, Shingles}} ->
%	    d("got shingles ~p\n",[Shingles]),
	    Sketch = shingles_to_sketch(HashSeed, Shingles),
%	    d("Id=~p sketch=~p\n",[Id,Sketch]),
	    Store2 = add_to_store(Id,Sketch,Store,DbInsertFn),
	    loop({HashSeed,Store2,DbInsertFn});

	M ->
	    d("unexpected ~p\n",[M]),
	    loop(State)

%    after 5000 ->
%	    d("timeout")
    end.

shingles_to_sketch(Seed, Shingles) ->
    Hashes = [ util:uhash(S,Seed) || S <- Shingles ],
    lists:min(Hashes).

add_to_store(Id,Sketch,Store,DbInsertFn) -> 
    case dict:is_key(Sketch,Store) of
	true ->
	    Set = dict:fetch(Sketch,Store),
	    case sets:is_element(Id,Set) of
		true -> % already an element, ignore
		    Store;
		_ ->
%		    d("emit new combo Sketch=~p Id=~p Others=~p\n",[Sketch,Id,length(sets:to_list(Set))]),
		    emit_new_combos(Id,Set,DbInsertFn),
		    dict:store(Sketch,sets:add_element(Id,Set),Store)
	    end;
	_ ->
	    dict:store(Sketch,
		       sets:add_element(Id,sets:new()),
		       Store)
    end.

emit_new_combos(Id1,Set,DbInsertFn) ->
    lists:foreach(
      fun(Id2) -> DbInsertFn(Id1, Id2) end,
      sets:to_list(Set)
     ).

% note: as long as Ids are increasing from file there is no need to check order, 
% Id1 will always be > Id2
%emit_combo(Id1,Id2) ->
%    io:format("sic ~p ~p\n",[Id2,Id1]).
