-module(main).
-compile(export_all).
-include("consts.hrl").
-include("debug.hrl").

main() ->
    spawn(etop,start,[]),
    start(),
    {ok,B} = file:read_file("test.data"),
    Lines = string:tokens(binary_to_list(B),"\n"),
%    process(["the cat sat","on the mat","in the tree","cat tree mat"],0).
    process(Lines).

process([]) ->
    dump();

process([Str|T]) ->
    { Id, Data } = parse:line(Str),
    d("processing ~p ~p\n",[Id,Data]),
    Shingles = util:shingles(Data),
%    d("id ~p shingles ~p\n",[Id,Shingles]),
    get(sketch_broadcast_router) ! { Id, {shingles,Shingles} },
%    timer:sleep(1),
    process(T).

dump() ->
    timer:sleep(5000),
    get(sic) ! dump,
    done.
 
start() ->
    put(ts, util:tostr()),
    put(sic, sketches_in_common:start()),
    put(sti, sketch_to_id:start(get(sic))),
    put(sketchers, [ sketcher:start(get(sti)) || _ <- lists:seq(1, ?SKETCH_SIZE)]),
    put(sketch_broadcast_router, broadcast_router:start(get(sketchers))).

%stop() ->
%    get(sketch_broadcast_router) ! stop.
	       
