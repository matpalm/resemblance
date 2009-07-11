-module(shingler).
-export([params/0,process/3]).
-include("debug.hrl").

params() ->
    opts:shingle_size().

process({Id,Content}, ShingleSize, EmitFn) ->
    Shingles = case length(Content) =< ShingleSize of
		   true ->
		       % single shingle padded to shingle size
		       [lists:flatten(io_lib:format("~-*s",[ShingleSize,Content]))];
		   false ->
		       util:shingles(Content, ShingleSize)
	       end,
    EmitFn({Id,Shingles}).

    



    
