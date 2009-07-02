-module(add_type_to_value).
-export([initial_state/0,process/3,finished/2]).

initial_state() ->
    0.

process({K,V}, _, EmitFn) ->
    Type = opts:atom_prop(type),    
    EmitFn({K,{Type,V}}),
    nil.

finished(_,_) ->
    nil.

	     
