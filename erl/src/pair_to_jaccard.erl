-module(pair_to_jaccard).
-compile(export_all).

start() ->
    IdName = dict:from_list(file_util:read_all_from_file(opts:string_prop(id_name))),
    IdPairs = file_util:read_all_from_file(opts:string_prop(id_pairs)),
    Out = bin_parser:open_file_for_write(opts:string_prop(output_file)),
    Type = opts:atom_prop(type),
    process(IdPairs, IdName, dict:new(), Out, Type),
    init:stop().

process([],_,_,_,_) ->
    done;

process([{Id1,Id2}|Pairs], IdName, IdShingles, Out, Type) ->
    { Shingles1,IdShingles2 } = shingle_for_id(Id1, IdShingles, IdName),
    { Shingles2,IdShingles3 } = shingle_for_id(Id2, IdShingles2, IdName),
    Jaccard = coeff:jaccard(Shingles1, Shingles2),
%    io:format("1 ~p ~p 2 ~p ~p Sim ~p\n",[Id1,Shingles1, Id2,Shingles2,Jaccard]),    
    bin_parser:write(Out, {{Id1,Id2},{Type,Jaccard}}),
%    io:format("~p ~p ~p\n",[Id1,Id2,Jaccard]),
    process(Pairs, IdName, IdShingles3, Out, Type).

shingle_for_id(Id, IdShingles, IdName) ->
    case dict:find(Id, IdShingles) of
	{ ok,Shingles } ->
	    { Shingles, IdShingles };
	_ ->
	    Name = dict:fetch(Id, IdName),
	    Shingles = util:shingles(Name),
	    IdShingles2 = dict:store(Id, Shingles, IdShingles),
	    { Shingles, IdShingles2 }
    end.
    
