-module(combos).
-export([params/0,process/3]).

params() ->
    nil.

process({_Key,VList}, _State, EmitFn) ->
    combos(VList, EmitFn).

combos(List,_) when length(List) < 2 ->
    done;

combos([H|T],EmitFn) ->
    all_pairs(H,T,EmitFn),
    combos(T, EmitFn).

all_pairs(_E,[],_EmitFn) ->
    done;

all_pairs(E,[H|T],EmitFn) ->
    PairInOrder = in_order(E,H),
    EmitFn({PairInOrder,1}),
    all_pairs(E,T,EmitFn).

in_order(A,B) ->
    case A < B of
	true  -> {A,B};
	false -> {B,A}
    end.
	     

