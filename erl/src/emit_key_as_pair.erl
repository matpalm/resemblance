-module(emit_key_as_pair).
-export([params/0,process/3]).

params() ->
    0.

process({{K1,K2},_}, _, EmitFn) ->
    EmitFn({K1,K2}).
%    EmitFn({K2,K1}).

