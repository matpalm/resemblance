-module(reducer).
-export([start/0]).

start() ->
    OutFilename = opts:string_prop(output_file,"top_N.out"),
    O = bin_parser:open_file_for_write(OutFilename),
    EmitFn = fun(X) -> bin_parser:write(O,X) end,

    Files = [ file_util:input_dir()++"/"++File || File <- file_util:input_files() ],

    Task = opts:task(),
    InitialState = apply(Task,initial_state,[]),
    ProcessFn = fun(Term,State) -> apply(Task,process,[Term,State,EmitFn]) end,
    FinalState = process(Files, InitialState, ProcessFn),
    apply(Task,finished,[FinalState, EmitFn]),
    
    file:close(O),
    init:stop().

process([], State, _ProcessFn) ->
    State;

process([File|Files], State, ProcessFn) ->
    State2 = parse_file(File, State, ProcessFn),
    process(Files, State2, ProcessFn).

parse_file(File, State, ProcessFn) ->
    F = bin_parser:open_file_for_read(File),
    parse_terms(F, State, ProcessFn).

parse_terms(F, State, ProcessFn) ->
    Read = bin_parser:read(F),
    case Read of 
	eof ->
	    State;
	{ok,Term} -> 
	    State2 = ProcessFn(Term, State),
	    parse_terms(F, State2, ProcessFn);
	Other ->
	    io:format("error, unexpected message ~p\n",[Other])
    end.
    
