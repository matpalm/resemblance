-module(shingler).
-export([initial_state/0,process/3,finished/2]).
-include("debug.hrl").

initial_state() ->
    nil.

process({Id,Content}, _, EmitFn) ->
    Shingles = util:shingles(Content),
    EmitFn({Id,Shingles}),
    nil;

process(X,_,_) ->
    io:format("unexpected process ~p\n",[X]).

finished(_,_) ->
     done.


    
