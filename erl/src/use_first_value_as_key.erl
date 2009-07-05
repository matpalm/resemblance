-module(use_first_value_as_key).
-export([params/0,process/3]).

params() ->
    nil.

process({_K,Values}, _, EmitFn) ->
    case length(Values) > 1 of
	true  -> EmitFn({hd(Values),tl(Values)});
	false -> ignore
    end.

