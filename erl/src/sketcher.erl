-module(sketcher).
-compile(export_all).
-include("debug.hrl").
-include("consts.hrl").

start(Receiver) ->
    HashSeed = util:uhash_seed(?SHINGLE_SIZE),
    spawn(?MODULE,loop,[HashSeed,Receiver]).

loop(Seed, Receiver) ->
%    d(">loop seed ~p\n",[Seed]),
    receive
	{Id, {shingles,Shingles}} ->
	    Hashes = [ util:uhash(S,Seed) || S <- Shingles ],
	    Sketch = lists:min(Hashes),
	    d("Id ~p => Hashes ~w Sketch ~p\n",[Id,Hashes,Sketch]),
	    Receiver ! {Id, {sketch,Sketch}};
	stop ->
	    Receiver ! stop,
%	    d("stopping\n"),
	    exit(0);
	M ->
	    d("sketcher unexpected ~p\n",[M])
    after 15000 ->
	    d("sketcher timeout\n"),
	    exit(1)
    end,
    loop(Seed,Receiver).

