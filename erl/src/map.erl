-module(map).
-compile(export_all).
-include("debug.hrl").

main() ->
 %    spawn(etop,start,[]),
    io:format("G\n"),
    wire_up_workers(),
    start_stats(),   
    TotalLines = parse_stdin(0),  
    get_workers_to_do_final_dump(TotalLines),
    wait_for_sketches_in_common_to_complete(),
    init:stop(),
    %start_candidate_calculation(),
    done.

parse_stdin(N) ->
    case io:get_line('') of 
	eof ->
	    N;
	Line ->
	    process_a_line(Line),
%	    potential_congestion_control_check(N),
	    potential_write_to_disk(N),
	    parse_stdin(N+1)
    end.

process_a_line(Str) ->
    { Id, Data } = parse_line(Str),
    Shingles = util:shingles(Data),
    [ Worker ! {Id,{shingles,Shingles}} || Worker <- get(workers)].

parse_line(Line) ->
    {ok,RE} = re:compile("^.*? "),
    {match,[{A,B}]} = re:run(Line,RE),
    Id = list_to_integer(string:substr(Line, A+1, B-1)),
    Data = string:substr(Line,B+1),
    {Id,Data}.

wire_up_workers() ->
    Workers = [ worker:start(N) || N <- lists:seq(1, opts:sketch_size()) ],
    put(workers, Workers).

start_stats() ->
    NamesAndPids = [ { workers, get(workers) } ],
    put(stats, stats:spawn_watcher(NamesAndPids)).

wait_for_sketches_in_common_to_complete() ->
    util:ack(workers).

get_workers_to_do_final_dump(TotalLines) ->
    [ W ! {final_dump,TotalLines} || W <- get(workers)].

potential_congestion_control_check(N) ->         
    case N rem opts:cc_check_freq() of
	0 -> 
	    d("line ~p\n",[N]),
	    stats:block_if_congested(get(stats));
	_ -> 
	    done
    end.

potential_write_to_disk(N) when N>0 ->         
    case N rem opts:write_to_disk_freq() of
	0 -> 
	    d("line ~p\n",[N]),
	    [ W ! {dump,N} || W <- get(workers)];
	_ -> 
	    done
    end;

potential_write_to_disk(_N) ->
    zero_case.

   

