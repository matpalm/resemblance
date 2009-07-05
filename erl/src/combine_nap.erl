-module(combine_nap).
-export([params/0,process/3]).

params() ->
    nil.

process({{Id1,Id2},Resemblances}, _, EmitFn) ->
    NAP = types([n,a,p],Resemblances,[]), % name address phone
    EmitFn({{Id1,Id2},NAP}).

types([], _, Acc) ->
    lists:reverse(Acc);

types([H|T], Resemblances, Acc) ->
    V = {H,value_for_type(H,Resemblances)},
    types(T,Resemblances,[V|Acc]).

value_for_type(_TargetType, []) ->
    0;

value_for_type(TargetType, [{Type,Resemblance}|Resemblances]) ->
    case TargetType == Type of
	true ->
	    Resemblance;
	false ->
	    value_for_type(TargetType, Resemblances)
    end.


