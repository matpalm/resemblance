-module(sum).
-export([params/0,process/3]).

params() ->
    case opts:int_prop(min_sum, -1) of
	-1 ->
	    fun(_) -> true end;
	N ->
	    fun(Sum) -> Sum > N end
    end.

process({Key,Values}, ShouldEmit, EmitFn) ->
    Sum = lists:sum(Values),
    case ShouldEmit(Sum) of
	true ->
	    EmitFn({Key,Sum});
	false ->
	    nothing
    end.




