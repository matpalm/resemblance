-module(top_N).
-export([initial_state/0,process/3,finished/2]).
-include("debug.hrl").

initial_state() ->
    [].

process(KV, [], _EmitFn) ->
    [KV];

process({_,Value}=KV, [{_, MinValue}|_]=TopKVs, _EmitFn) ->
    case length(TopKVs) < num_to_keep() of
	true ->
	    insert_value(KV, TopKVs);
	false ->
	    case Value =< MinValue of
		true -> 
		    TopKVs;
		false ->
		    ensure_not_too_many_elems(insert_value(KV, TopKVs))
	    end
    end.

ensure_not_too_many_elems(TopKVs) ->
    case length(TopKVs) > num_to_keep() of
	true ->  tl(TopKVs);
	false -> TopKVs
    end.

insert_value(KV, TopKVs) ->
    insert_value(KV, TopKVs, []).

insert_value(KV, [], List) ->
    lists:reverse(List) ++ [KV];

insert_value({_,NewV}=KV, [{_,TopV}=TopKV|OtherTopKVs]=TopKVs, List) ->
    case NewV =< TopV of
	true ->
	    lists:reverse(List) ++ [KV] ++ TopKVs;
	false ->
	    insert_value(KV, OtherTopKVs,  [TopKV|List])
    end.

finished(State,EmitFn) ->
    lists:foreach(EmitFn, State).

num_to_keep() ->
    opts:int_prop(num_to_keep,10).
