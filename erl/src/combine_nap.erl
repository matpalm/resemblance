-module(combine_nap).
-export([initial_state/0,process/3,finished/2]).

initial_state() ->
    nop.

process({{Id1,Id2},Resemblances}, _, _EmitFn) ->
    io:format("~p ~p ",[Id1,Id2]),
    print_for_types([n,a,p],Resemblances), % name address phone
    io:format("\n").

finished(_,_) ->
    nop.

print_for_types([], _) ->
    done;

print_for_types([H|T], Resemblances) ->
    io:format("~p ", [value_for_type(H,Resemblances)]),
    print_for_types(T,Resemblances).

value_for_type(_TargetType, []) ->
    0;

value_for_type(TargetType, [{Type,Resemblance}|Resemblances]) ->
    case TargetType == Type of
	true ->
	    Resemblance;
	false ->
	    value_for_type(TargetType, Resemblances)
    end.


