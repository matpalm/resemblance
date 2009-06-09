-module(sketches_in_common).
-export([start/1,loop/1,emit_all_above/3,routing_fn/1]).
-include("debug.hrl").

start(Node) ->
    spawn(Node,?MODULE,loop,[dict:new()]).

loop(Freq) ->
%    d(">loop ~p ~p\n",[process_info(self(),message_queue_len),dict:size(Freq)]),
    receive
	{ack,Pid} ->
	    Pid ! {ack,self()},
	    loop(Freq);

	dump ->
	    Counts = lists:sort([ Count || {_,Count} <- dict:to_list(Freq) ]),
	    N = length(Counts),
	    case N of 
		0 -> d("count: size=0 min=0 median=0 mean=0 max=0\n");
		_ -> d("count: size=~p min=~p median=~p mean=~p max=~p\n",
		       [N, hd(Counts), lists:nth(N div 2,Counts), lists:sum(Counts)/N, lists:nth(N,Counts)])
	    end,
	    CountsFreq = dict:to_list(lists:foldl(fun(I,D) -> dict:update_counter(I,1,D) end, dict:new(), Counts)),
	    Values = [ Freq || {_Val,Freq} <- lists:keysort(1,CountsFreq) ],
	    d("freqs ~w\n",[Values]),
	    loop(Freq);

	{ send_to_coeff_calculator, Calculator } ->
	    emit_all_above(Calculator, dict:to_list(Freq), opts:sketches_in_common_cutoff()),
	    util:ack(Calculator),
	    init:stop(),
	    loop(Freq);

	{ sketch_in_common, Id1, Id2 } ->
%	    d("{ sketch_in_common, ~p, ~p }\n",[Id1,Id2]),
	    Key = ensure_first_less_than(Id1,Id2),
	    Freq2 = dict:update_counter(Key, 1, Freq),
	    loop(Freq2);

	  M -> 
	      d("unexpected msg ~p\n",[M]),
	      loop(Freq)
    
    end.

ensure_first_less_than(A,B) when A > B -> { B,A };
ensure_first_less_than(A,B) -> { A,B }.
    
%freqs_as_list(Freq) ->
%    lists:reverse(lists:keysort(2,dict:to_list(Freq))).
    
emit_all_above(_Calculator, [], _Cutoff) ->
    done;
emit_all_above(Calculator, [{{Id1,Id2},NumCommon}|T], Cutoff) ->
    case NumCommon >= Cutoff of
	true -> Calculator ! { jaccard_coeff_for, Id1, Id2, NumCommon };
	false -> do_nothing
    end,
    emit_all_above(Calculator, T, Cutoff).    
    
routing_fn(Pids) ->
    fun({ sketch_in_common, Id1, Id2 }=Msg) -> 
	    Idx = (Id1+Id2) rem length(Pids),
%	    d("route ~w to ~w\n",[Msg,Idx]),
	    lists:nth(Idx+1, Pids) ! Msg
    end.
		      

