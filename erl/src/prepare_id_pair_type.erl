-module(prepare_id_pair_type).
-compile(export_all).

parse_line(Line) ->    
    [Id1,Id2] = [ list_to_integer(binary_to_list(X)) || X <- re:split(Line," ") ],
    Type = opts:atom_prop(type),
    case Type of
	nap -> {{Id1,Id2},[{names,1},{addresses,1},{phones,1}]};
	_   -> {{Id1,Id2},{Type,1}}
    end.
