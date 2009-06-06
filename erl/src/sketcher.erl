-module(sketcher).
-compile(export_all).
-include("debug.hrl").
-include("consts.hrl").

start(Node,ReceiverFn) ->
    HashSeed = util:uhash_seed(?SHINGLE_SIZE),
    spawn(Node,?MODULE,loop,[HashSeed,ReceiverFn]).

loop(Seed, ReceiverFn) ->
%    d(">loop seed ~p\n",[Seed]),
    receive
	{ack,Pid} ->
	    Pid ! {ack,self()},
	    loop(Seed, ReceiverFn);

	{Id, {shingles,Shingles}} ->
	    Hashes = [ util:uhash(S,Seed) || S <- Shingles ],
	    Sketch = lists:min(Hashes),
%	    d("Id ~p => Hashes ~w Sketch ~p\n",[Id,Hashes,Sketch]),
	    ReceiverFn({Id, {sketch,Sketch}});

	M ->
	    d("unexpected ~p\n",[M])

    end,
    loop(Seed,ReceiverFn).

