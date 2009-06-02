-module(sketches_in_common).
-export([start/0,loop/1,emit_all_above/3,routing_fn/1]).
-include("debug.hrl").
-include("consts.hrl").

start() ->
    spawn(?MODULE,loop,[dict:new()]).

loop(Freq) ->
%    d(">loop ~p ~p\n",[process_info(self(),message_queue_len),dict:size(Freq)]),
    receive
	{ack,Pid} ->
	    Pid ! {ack,self()},
	    loop(Freq);

	dump ->
	    FreqList = dict:to_list(Freq),
	    d("number sketches in common entries ~p\n",[length(FreqList)]), 

	    TotalInCommon = lists:sum(lists:map(
			      fun({_Ids,Count}) -> Count end,
			      FreqList)),
	    d("number sketches in common total count ~p\n",[TotalInCommon]), 

	    d("freqs=~w\n",[lists:sublist(freqs_as_list(Freq),10)]),
	    loop(Freq);

	{ send_to_coeff_calculator, Calculator } ->
	    emit_all_above(Calculator, dict:to_list(Freq), ?SKETCHES_IN_COMMON_CUTOFF),
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
    
freqs_as_list(Freq) ->
    lists:reverse(lists:keysort(2,dict:to_list(Freq))).
    
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
		      

