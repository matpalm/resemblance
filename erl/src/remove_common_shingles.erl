-module(remove_common_shingles).
-export([initial_state/0,process/3,finished/2]).

initial_state() ->
    CommonFilename = opts:string_prop(common_file),
    CommonShingles = [ Sh || {Sh,_Freq} <- file_util:read_all_from_file(CommonFilename)],
    sets:from_list(CommonShingles).

process({Key,Values}, CommonShingles, EmitFn) ->
    Shingles = sets:from_list(Values),
    CleanShingles = sets:subtract(Shingles, CommonShingles),
    
    case sets:size(CleanShingles) == sets:size(Shingles) of
	false ->
	    io:format("common   ~p ~p\n",[sets:size(CommonShingles), lists:sort(sets:to_list(CommonShingles))]),
	    io:format("shingles ~p ~p\n",[sets:size(Shingles),       lists:sort(sets:to_list(Shingles))]),
	    io:format("clean    ~p ~p\n",[sets:size(CleanShingles),  lists:sort(sets:to_list(CleanShingles))]);
	true ->
	    skinnnnner
    end,

    EmitFn({Key,sets:to_list(CleanShingles)}),
    CommonShingles.

finished(_,_) ->
    nil.

