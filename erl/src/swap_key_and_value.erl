-module(swap_key_and_value).
-export([initial_state/0,process/3,finished/2]).

initial_state() ->
    0.

process({K,V}, _, EmitFn) ->
    EmitFn({V,K}),
    nil.

finished(_,_) ->
    nil.

	     


