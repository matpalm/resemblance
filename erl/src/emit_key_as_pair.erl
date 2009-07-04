-module(emit_key_as_pair).
-export([initial_state/0,process/3,finished/2]).

initial_state() ->
    0.

process({{K1,K2},_}, _, EmitFn) ->
    EmitFn({K1,K2}),       
    EmitFn({K2,K1}),       
    nil.

finished(_,_) ->
    nil.
