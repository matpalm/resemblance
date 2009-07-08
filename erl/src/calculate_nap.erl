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
    Weights = dict:from_list([{names,NameWeightN},{addresses,AddressWeightN},{phones,PhoneWeightN}]),
    process(F,Weights),

    init:stop().

process(F, WeightsDict) ->
    Read = bin_parser:read(F),
    case Read of
	eof ->
	    done;
	{ok,Term} ->
	    {{Id1,Id2},Resems} = Term,
	    Weight = calc_overall_weight(Resems, WeightsDict, 0),
	    io:format("~p ~p ~p\n",[Id1,Id2,Weight]),
	    process(F, WeightsDict)
    end.
    
calc_overall_weight([], _WeightsDict, Acc) ->
    Acc;

calc_overall_weight([{Type,Resem}|TypeResems], WeightsDict, Acc) ->
    WeightedSum = dict:fetch(Type, WeightsDict) * Resem,
    calc_overall_weight(TypeResems, WeightsDict, WeightedSum + Acc).


      
    

