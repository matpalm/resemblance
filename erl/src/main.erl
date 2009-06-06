-module(main).
-compile(export_all).
-include("consts.hrl").
-include("debug.hrl").

-define(CC_CHECK_FREQ, 100). % lines
-define(NUM_SKETCH_IN_COMMONS, 8).
-define(NUM_SKETCH_TO_IDS, 8).

restart_slaves() ->
    erl_boot_server:start(["192.168.0.2"]),
    net_adm:world(),
    [ rpc:call(Node,init,restart,[]) || Node <- nodes() ].

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
	[ sketches_in_common:start(next_node()) || _ <- lists:seq(1, ?NUM_SKETCH_IN_COMMONS) ],
    put(sketches_in_commons, SketchesInCommonPids),

    SketchesInCommonPidsRoutingFn = sketches_in_common:routing_fn(SketchesInCommonPids),
    SketchToIdPids =
	[ sketch_to_id:start(next_node(), SketchesInCommonPidsRoutingFn) || _ <- lists:seq(1, ?NUM_SKETCH_TO_IDS) ],
    put(sketch_to_ids, SketchToIdPids),

    SketchToIdPidsRoutingFn = sketch_to_id:routing_fn(SketchToIdPids),
    SketcherPids =
	[ sketcher:start(next_node(), SketchToIdPidsRoutingFn) || _ <- lists:seq(1, ?SKETCH_SIZE) ],
    put(sketchers, SketcherPids).   

start_stats() ->
    NamesAndPids = [ 
		     { sketch_to_id, get(sketch_to_ids) },
		     { sketches_in_common, get(sketches_in_commons) }
		    ],
    put(stats, stats:spawn_watcher(NamesAndPids)).

next_node() ->
    case node() of
	'nonode@nohost' ->
	    node(); % running as single node, spawn everything locally
	_ ->
	    case get(nodes) of
		undefined -> next_node([]);
		Nodes -> next_node(Nodes)
	    end
    end.

next_node([]) ->
    next_node(net_adm:world());
next_node([H|T]) ->
    put(nodes, T),
    H.
	     
wait_for_sketches_in_common_to_complete() ->
    util:ack(sketchers),
    util:ack(sketch_to_ids),
%    hd(get(sketches_in_commons)) ! dump,
    util:ack(sketches_in_commons).
    
start_candidate_calculation() ->
    %get(shingle_store) ! dump,
    [ P ! { send_to_coeff_calculator, get(shingle_store) } || P <- get(sketches_in_commons)]. 

potential_congestion_control_check(N) ->         
    case N rem ?CC_CHECK_FREQ of
	0 -> 
	    d("line ~p\n",[N]),
	    stats:block_if_congested(get(stats));
	_ -> 
	    done
    end.


   

