-module(main).
-compile(export_all).
-include("consts.hrl").
-include("debug.hrl").

main() ->
    spawn(etop,start,[]),
    start(),
    process_file("test.data"),
    wait_for_sketches_in_common_to_complete(),
    start_candidate_calculation(),
    done.

process_file(File) ->
    {ok,B} = file:read_file(File),
    Lines = string:tokens(binary_to_list(B),"\n"),
    forward_all_lines(Lines).
    
forward_all_lines([]) ->
    done;

forward_all_lines([Str|T]) ->
    { Id, Data } = parse:line(Str),
    %d("processing ~p ~p\n",[Id,Data]),
    Shingles = util:shingles(Data),
    %d("id ~p shingles ~p\n",[Id,Shingles]),
    get(sketch_broadcast_router) ! { Id, {shingles,Shingles} },
    get(shingle_store) ! { Id, {shingles,Shingles} },
%    timer:sleep(1),
    forward_all_lines(T).

start() ->
    put(shingle_store, shingle_store:start()),
    put(sketches_in_common, sketches_in_common:start()),
    put(sketch_to_id, sketch_to_id:start(get(sketches_in_common))),
    put(sketchers, [ sketcher:start(get(sketch_to_id)) || _ <- lists:seq(1, ?SKETCH_SIZE)]),
    put(sketch_broadcast_router, broadcast_router:start(get(sketchers))).

wait_for_sketches_in_common_to_complete() ->
    util:ack(sketch_broadcast_router),
    util:ack(sketchers),
    util:ack(sketch_to_id),
    util:ack(sketches_in_common).
    
start_candidate_calculation() ->
    %get(shingle_store) ! dump,
    get(sketches_in_common) ! { send_to_coeff_calculator, get(shingle_store) }.
    
	       
