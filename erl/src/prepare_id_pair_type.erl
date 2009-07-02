-module(prepare_id_pair_type).
-compile(export_all).

%TODO: no longer used?
parse_line(Line) ->    
    [Id1S,Id2S,ResS] = [ binary_to_list(X) || X <- re:split(Line," ") ],
    Id1 = list_to_integer(Id1S),
    Id2 = list_to_integer(Id2S),
    Res = list_to_float(ResS),
    Type = opts:atom_prop(type),
    {{Id1,Id2},{Type,Res}}.
