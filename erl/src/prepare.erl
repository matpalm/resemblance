-module(prepare).
-compile(export_all).

start() ->
    Files = open_files(),
    parse_stdin(Files),
    close_files(Files),
    init:stop().

open_files() ->
    NumFiles = opts:num_files(),
    Filenames = filenames_for(NumFiles),
    [ bin_parser:open_file_for_write(Filename) 
      || Filename <- Filenames ].

filenames_for(1) ->
    [ opts:string_prop(output_file) ];

filenames_for(NumFiles) ->
    file_util:ensure_output_dir_created(),
    [ file_util:output_dir()++"/"++integer_to_list(N)
      || N <- lists:seq(0,NumFiles-1)].

close_files(Files) ->
    lists:foreach(
      fun(F) -> file:close(F) end,
      Files
     ).

parse_stdin(Files) ->
    ParserMod = opts:atom_prop(parser),
    parse_stdin([],Files,ParserMod).

parse_stdin([],Files,ParserMod) ->
    parse_stdin(Files,Files,ParserMod);

parse_stdin([F|T],Files,ParserMod) ->
    case io:get_line('') of 
	eof ->  
	    done;
	Line -> 
	    Parsed = apply(ParserMod,parse_line,[chomp(Line)]),
	    bin_parser:write(F, Parsed),
	    parse_stdin(T,Files,ParserMod)
    end.
    
chomp(S) -> 
    string:substr(S,1,length(S)-1).

