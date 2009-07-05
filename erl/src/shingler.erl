-module(shingler).
-export([params/0,process/3]).
-include("debug.hrl").

params() ->
    nil.

process({Id,Content}, _, EmitFn) ->
    Shingles = util:shingles(Content),
    EmitFn({Id,Shingles}).




    
