-module(calculate_nap).
-compile(export_all).

start() ->
    File = opts:string_prop(file),

    NameWeight = opts:int_prop(n,0),
    AddressWeight = opts:int_prop(a,0),
    PhoneWeight = opts:int_prop(p,0),

    Total = NameWeight + AddressWeight + PhoneWeight,
    case Total of 
	0 -> erlang:error(total_sum_of_weights_cant_be_zero);
	_ -> ok
    end,

    NameWeightN = NameWeight / Total,
    AddressWeightN = AddressWeight / Total,
    PhoneWeightN = PhoneWeight / Total,

    F = bin_parser:open_file_for_read(File),
    Weights = [{n,NameWeightN},{a,AddressWeightN},{p,PhoneWeightN}],
    process(F,Weights),

    init:stop().

process(F,Weights) ->
    Read = bin_parser:read(F),
    case Read of
	eof ->
	    done;
	{ok,Term} ->
	    calc_overall_weight(Term,Weights),
	    process(F, Weights)
    end.

calc_overall_weight({{Id1,Id2},Resems}, Weights) ->
    WeightedSum = lists:sum([ R*W || {{_T,R},{_T,W}} <- lists:zip(Resems,Weights) ]),
    io:format("~p ~p ~p\n",[Id1,Id2,WeightedSum]).


      
    

