-module(explode_combos).
-compile(export_all).

% TODO: works but explode step needs serious cleanup...

start() ->
    In = bin_parser:open_file_for_read(opts:string_prop(input_file)),
    DupIds = dict:from_list(file_util:read_all_from_file(opts:string_prop(dup_ids))),
    Out = bin_parser:open_file_for_write(opts:string_prop(output_file)),

    ExplodeFn = fun(R) -> explode(R,DupIds,Out) end,
    process(In, ExplodeFn),

    file:close(Out),
    init:stop().

process(In, ExplodeFn) ->
    Read = bin_parser:read(In),
    case Read of
	eof ->
	    done;
	{ok,Res} ->
	    ExplodeFn(Res),
	    process(In, ExplodeFn)
    end.

explode({IdPair,Result}, DupIds, Out) ->
    io:format("exploding for ~p\n",[IdPair]),
    Combos = [IdPair],
    io:format("combos ~p\n",[Combos]),
    Combos2 = Combos ++ explode_ids(lhs, Combos, DupIds),
    io:format("combos2 ~p\n",[Combos2]),
    Combos3 = Combos2 ++ explode_ids(rhs, Combos2, DupIds),
    io:format("combos3 ~p\n",[Combos3]),
    ReorderedCombos = [ ensure_ordered(Pair) || Pair <- Combos3],
    lists:foreach(
      fun(C) -> bin_parser:write(Out, {C,Result}) end,
      ReorderedCombos
     ).

explode_ids(lhs, List, DupIds) ->
    io:format("explode_ids lhs ~p \n",[List]),
    lists:flatten([ explode_id_pair(IdPair, DupIds) || IdPair <- List ]);

explode_ids(rhs, List, DupIds) ->
    io:format("explode_ids rhs ~p \n",[List]),
    Flipped = [{B,A} || {A,B} <- List],
    Exploded = explode_ids(lhs, Flipped, DupIds),
    [ {A,B} || {B,A} <- Exploded ].

explode_id_pair({Id1,Id2}, DupIds) ->
    case dict:find(Id1, DupIds) of
	{ok, Dups} ->
	    E = [ {Dup,Id2} || Dup <- Dups ],
	    io:format("explode id ~p ~p -> E=~p\n",[Id1,Id2,E]),
	    E;
	_ ->
	    []
    end.   

ensure_ordered({A,B}) ->
    case A < B of
	true  -> {A,B};
	false -> {B,A}
    end.
	   
    
