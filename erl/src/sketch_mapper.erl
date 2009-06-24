-module(sketch_mapper).
-export([start_fn/0,map/3]).
-include("debug.hrl").

start_fn() ->
    Seeds = [ util:uhash_seed(opts:shingle_size()) 
	      || _ <- lists:seq(1,opts:sketch_size()) ],    
    NewWorkerFn = 
	fun(InFile,OutFile) ->
		spawn(?MODULE,map,[InFile,OutFile,Seeds])
	end,
    NewWorkerFn.

map(InputFile,OutputFile,Seeds) ->
    Lines = file_util:read(InputFile),
    Result = process(Lines,Seeds),
    file_util:write(OutputFile,Result),
    map_reduce:worker_done().

process(Lines,Seeds) ->
    process(Lines,Seeds,[]).

process([],_Seeds,Acc) ->
    Acc;

process([{Id,Content}|Lines],Seeds,Acc) ->
    Shingles = util:shingles(Content),
    Sketches = shingles_to_sketches(Seeds, Shingles), 
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

    
