-module(emit_values).
-export([initial_state/0,process/3,finished/2]).

initial_state() ->
    nil.

process({_Key,Values}, _State, EmitFn) ->
    lists:foreach(fun(V) -> EmitFn({V,1}) end, Values),
    nil.

finished(_,_) ->
    nil.

