-module(sketcher).
-export([params/0,process/3]).

params() ->
    [ util:uhash_seed(opts:shingle_size()) 
	      || _ <- lists:seq(1,opts:sketch_size()) ].

process({Id,Shingles}, Seeds, EmitFn) ->
    Sketches = shingles_to_sketches(Seeds, Shingles), 
    lists:foreach(fun(Sketch) -> EmitFn({Sketch,Id}) end, Sketches),
    Seeds.

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
