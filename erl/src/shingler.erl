-module(shingler).
-export([params/0,process/3]).
-include("debug.hrl").

params() ->
    opts:shingle_size().

process({Id,Content}, ShingleSize, EmitFn) ->
    Shingles = util:shingles(Content, ShingleSize),
    EmitFn({Id,Shingles}).




    
