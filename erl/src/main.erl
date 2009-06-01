-module(main).
-compile(export_all).
-include("consts.hrl").
-include("debug.hrl").

main() ->
%    spawn(etop,start,[]),

    wire_up_workers(),

    parse:each_line(
      "test.data",
      fun(Line) -> process_a_line(Line) end
      ),

    wait_for_sketches_in_common_to_complete(),
    start_candidate_calculation(),
    done.

process_a_line(Str) ->
    { Id, Data } = parse:parse_line(Str),
    %d("processing ~p ~p\n",[Id,Data]),
    Shingles = util:shingles(Data),
    %d("id ~p shingles ~p\n",[Id,Shingles]),
    [ Sketcher ! {Id,{shingles,Shingles}} || Sketcher <- get(sketchers)],
    get(shingle_store) ! { Id, {shingles,Shingles} }.

wire_up_workers() ->
    put(shingle_store, shingle_store:start()),
    
    SketchesInCommonPids = 
	[ sketches_in_common:start() || _ <- lists:seq(1, ?NUM_SKETCH_IN_COMMONS) ],
    put(sketches_in_commons, SketchesInCommonPids),

    SketchToId =
	sketch_to_id:start(sketch_in_common_routing_fn(SketchesInCommonPids)),
    put(sketch_to_id, SketchToId),

    SketcherPids =
	[ sketcher:start(get(sketch_to_id)) || _ <- lists:seq(1, ?SKETCH_SIZE) ],
    put(sketchers, SketcherPids),   

    stats:spawn_watcher(
      SketchesInCommonPids ++ [SketchToId],
      lists:duplicate(?NUM_SKETCH_IN_COMMONS,sketch_in_common) ++ [sketch_to_id]
     ),

    done.
    
    

%    put(sketch_broadcast_router, broadcast_router:start(get(sketchers))).

wait_for_sketches_in_common_to_complete() ->
%   util:ack(sketch_broadcast_router),
    util:ack(sketchers),
    util:ack(sketch_to_id),
    util:ack(sketches_in_commons).
    
start_candidate_calculation() ->
    %get(shingle_store) ! dump,
    [ P ! { send_to_coeff_calculator, get(shingle_store) } || P <- get(sketches_in_commons)]. 
         
sketch_in_common_routing_fn(Pids) ->
    fun({ sketch_in_common, Id1, Id2 }=Msg) -> 
	    Idx = (Id1+Id2) rem length(Pids),
	    lists:nth(Idx+1, Pids) ! Msg
    end.
