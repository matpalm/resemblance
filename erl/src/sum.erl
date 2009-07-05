-module(sum).
-export([params/0,process/3]).

params() ->
    nil.

process({Key,Values}, _State, EmitFn) ->
    Sum = lists:sum(Values),
    case should_emit(Sum) of
	true ->
	    EmitFn({Key,Sum});
	false ->
	    nothing
    end.

should_emit(Sum) ->
    case opts:int_prop(min_sum, -1) of
	-1 -> 
	    true; % no filter, always emit
	N  -> 
	    Sum > N
    end.
	     

