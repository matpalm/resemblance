-module(explode_combos).
-compile(export_all).

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

explode({{Id1,Id2}=T,Result}, DupIds, Out) ->
    Combos = lists:flatten([
	       [T],
	       explode_ids(Id1,Id2,DupIds),
	       explode_ids(Id2,Id1,DupIds)
	       ]),
    ReorderedCombos = [ ensure_order(Pair) || Pair <- Combos],
    lists:foreach(
      fun(C) -> bin_parser:write(Out, {C,Result}) end,
      ReorderedCombos
     ).

explode_ids(Id1, Id2, DupIds) ->
    case dict:find(Id1, DupIds) of
	{ok, Dups} ->
	    [ {Dup,Id2} || Dup <- Dups ];
	_ ->
	    []
    end.

ensure_order({A,B}) ->
    case A < B of
	true  -> {A,B};
	false -> {B,A}
    end.
	   
    
