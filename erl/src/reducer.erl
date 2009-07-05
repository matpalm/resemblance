-module(reducer).
-export([start/0]).

start() ->
    OutFilename = opts:string_prop(output_file,"top_N.out"),
    O = bin_parser:open_file_for_write(OutFilename),
    EmitFn = fun(X) -> bin_parser:write(O,X) end,
    Files = file_util:input_files(),
    Task = opts:task(),
    io:format("OutFilename ~p Files ~p Task ~p\n",[OutFilename,Files,Task]),
    Params = apply(Task,params,[]),
    ProcessFn = fun(Term) -> apply(Task,process,[Term,Params,EmitFn]) end,
    process(Files, ProcessFn),  
    file:close(O),
    init:stop().

process([], _ProcessFn) ->
    done;

process([File|Files], ProcessFn) ->
    parse_file(File, ProcessFn),
    process(Files, ProcessFn).

parse_file(File, ProcessFn) ->
    F = bin_parser:open_file_for_read(File),
    parse_terms(F, ProcessFn),
    file:close(F).

parse_terms(F, ProcessFn) ->
    Read = bin_parser:read(F),
    case Read of 
	eof ->
	    done;
	{ok,Term} -> 
	    ProcessFn(Term),
	    parse_terms(F, ProcessFn);
	Other ->
	    io:format("error, unexpected message ~p\n",[Other])
    end.
    
