-module(add_type_to_value).
-export([params/0,process/3]).

params() ->
    nil.

process({K,V}, _, EmitFn) ->
    Type = opts:atom_prop(type),    
    EmitFn({K,{Type,V}}).


	     
