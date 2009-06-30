-module(total_sum).
-export([initial_state/0,process/3,finished/2]).

initial_state() ->
    0.

process({_,Values}, AccSum, _EmitFn) ->
    Sum = case is_list(Values) of
	      true -> lists:sum(Values);
	      false -> Values
	  end,   
    AccSum + Sum.

finished(Sum,EmitFn) ->
    EmitFn({total,Sum}).

	     


