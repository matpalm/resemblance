-module(filter_single_value).
-export([params/0,process/3]).

params() ->
    nil.

process({K,Values}, _, EmitFn) ->
    case length(Values) > 0 of
	true  -> EmitFn({K,hd(Values)});
	false -> ignore
    end.

