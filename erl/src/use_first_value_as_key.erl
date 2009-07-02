-module(use_first_value_as_key).
-export([initial_state/0,process/3,finished/2]).

initial_state() ->
    0.

process({_K,Values}, _, EmitFn) ->
    case length(Values) > 1 of
	true  -> EmitFn({hd(Values),tl(Values)});
	false -> ignore
    end,
    nil.

finished(_,_) ->
    nil.
