-module(prepare_id_list_type).
-compile(export_all).

parse_line(Line) ->    
    List = [ list_to_integer(binary_to_list(X)) || X <- re:split(Line," ") ],
    Type = opts:atom_prop(type),
    Pairs = lists:flatten(combos(List, [])),
    PairsWithType = [ {P,{Type,1}} || P <- Pairs ],
    { multiple_values, PairsWithType }.

combos([],Acc) ->
    Acc;

combos([Id1|Ids],Acc) ->
    Pairs = [ {Id1, Id2} || Id2 <- Ids ],
    combos(Ids,[Pairs|Acc]).
    

