-module(main).
-compile(export_all).
-include("consts.hrl").
-include("debug.hrl").

main() ->
    start(),
%    process(["the cat sat","on the mat","in the tree","cat tree mat"],0).
    process(["the cat sat","in a tree","sat on mat","under a tree"],0).

process([],_) ->
    timer:sleep(5),
    stop();
process([Str|T],Id) ->
    d("processing ~p ~p\n",[Id,Str]),
    Shingles = util:shingles(Str),
    d("id ~p shingles ~p\n",[Id,Shingles]),
    get(sketch_rr_router) ! { Id, {shingles,Shingles} },
%    timer:sleep(1),
    process(T,Id+1).
 
start() ->
    put(ts, util:tostr()),
    put(sti, sketch_to_id:start(get(ts))),
    put(sketchers, [ sketcher:start(get(sti)) || _ <- lists:seq(1, ?SHINGLE_SIZE)]),
    put(sketch_rr_router, rr_router:start(get(sketchers))).

stop() ->
    get(sketch_rr_router) ! stop.
%    [ get(P) ! stop || P <- [ts,sti,sketch_rr_router] ],
%    [ P ! stop || P <- get(sketchers) ].
	       
