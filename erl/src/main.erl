-module(main).
-compile(export_all).
-include("debug.hrl").

main() ->
 %    spawn(etop,start,[]),

    wire_up_workers(),
    start_stats(),

    parse:each_line(
      "test.data",
      fun(Line, N) -> 
	      potential_congestion_control_check(N),
	      process_a_line(Line) 
      end
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
	[ sketches_in_common:start(mnode:next_node()) || _ <- lists:seq(1, opts:num_sketches_in_common()) ],
    put(sketches_in_commons, SketchesInCommonPids),

    SketchesInCommonPidsRoutingFn = sketches_in_common:routing_fn(SketchesInCommonPids),
    SketchToIdPids =
	[ sketch_to_id:start(mnode:next_node(), SketchesInCommonPidsRoutingFn) || _ <- lists:seq(1, opts:num_sketch_to_id()) ],
    put(sketch_to_ids, SketchToIdPids),

    SketchToIdPidsRoutingFn = sketch_to_id:routing_fn(SketchToIdPids),
    SketcherPids =
	[ sketcher:start(mnode:next_node(), SketchToIdPidsRoutingFn) || _ <- lists:seq(1, opts:sketch_size()) ],
    put(sketchers, SketcherPids).   

start_stats() ->
    NamesAndPids = [ 
		     { sketchers, get(sketchers) },
		     { sketch_to_id, get(sketch_to_ids) },
		     { sketches_in_common, get(sketches_in_commons) }
		    ],
    put(stats, stats:spawn_watcher(NamesAndPids)).

wait_for_sketches_in_common_to_complete() ->
    util:ack(sketchers),
    util:ack(sketch_to_ids),
%    hd(get(sketches_in_commons)) ! dump,
    util:ack(sketches_in_commons).
    
start_candidate_calculation() ->
    %get(shingle_store) ! dump,
    [ P ! { send_to_coeff_calculator, get(shingle_store) } || P <- get(sketches_in_commons)]. 

potential_congestion_control_check(N) ->         
    case N rem opts:cc_check_freq() of
	0 -> 
	    d("line ~p\n",[N]),
	    stats:block_if_congested(get(stats));
	_ -> 
	    done
    end.


   

