-module(swap_key_and_value).
-export([params/0,process/3]).

params() ->
    nil.

process({K,V}, _, EmitFn) ->
    EmitFn({V,K}).

	     


