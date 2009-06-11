-module(main).
-compile(export_all).
-include("debug.hrl").

main() ->
 %    spawn(etop,start,[]),
    db:start(),

    wire_up_workers(),
    start_stats(),
    
    InsertDocumentFn = db:insert_document_fn(),
    parse:each_line(
      "test.data",
      fun(Line, N) -> 
	      potential_congestion_control_check(N),
	      process_a_line(Line, InsertDocumentFn) 
      end
      ),

    wait_for_sketches_in_common_to_complete(),
    %start_candidate_calculation(),
    init:stop(),
    done.

process_a_line(Str, InsertDocumentFn) ->
    { Id, Data } = parse:parse_line(Str),
    InsertDocumentFn(Id,Data),
    Shingles = util:shingles(Data),
    [ Worker ! {Id,{shingles,Shingles}} || Worker <- get(workers)].

wire_up_workers() ->
    DbInsertFn = db:insert_sketch_in_common_fn(),
    Workers = [ worker:start(DbInsertFn) || _ <- lists:seq(1, opts:sketch_size()) ],
    put(workers, Workers).

start_stats() ->
    NamesAndPids = [ 
		     { workers, get(workers) }
		    ],
    put(stats, stats:spawn_watcher(NamesAndPids)).

wait_for_sketches_in_common_to_complete() ->
    util:ack(workers).

potential_congestion_control_check(N) ->         
    case N rem opts:cc_check_freq() of
	0 -> 
	    d("line ~p\n",[N]),
	    stats:block_if_congested(get(stats));
	_ -> 
	    done
    end.


   

