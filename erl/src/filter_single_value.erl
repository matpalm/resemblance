-module(filter_single_value).
-export([initial_state/0,process/3,finished/2]).

initial_state() ->
    0.

process({K,Values}, _, EmitFn) ->
    case length(Values) > 0 of
	true  -> EmitFn({K,hd(Values)});
	false -> ignore
    end,
    nil.

finished(_,_) ->
    nil.
