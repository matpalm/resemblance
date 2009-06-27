-module(prepare).
-compile(export_all).

start() ->
    Files = open_files(),
    parse_stdin(Files),
    close_files(Files),
    init:stop().

open_files() ->
    file_util:ensure_output_dir_created(),
    NumFiles = opts:int_prop(num_files,10),
    Filenames = [ file_util:output_dir()++"/"++integer_to_list(N)++".gz"
		  || N <- lists:seq(1,NumFiles)],
    [ bin_parser:open_file_for_write(Filename) 
      || Filename <- Filenames ].

close_files(Files) ->
    lists:foreach(
      fun(F) -> file:close(F) end,
      Files
     ).

parse_stdin(Files) ->
    parse_stdin([],Files).

parse_stdin([],Files) ->
    parse_stdin(Files,Files);

parse_stdin([F|T],Files) ->
    case io:get_line('') of 
	eof ->  
	    done;
	Line -> 
	    Parsed = parse_line(chomp(Line)),
	    bin_parser:write(F, Parsed),
	    parse_stdin(T,Files)
    end.
    
chomp(S) -> 
    string:substr(S,1,length(S)-1).

parse_line(Line) ->
    {ok,RE} = re:compile("^.*? "),
    {match,[{A,B}]} = re:run(Line,RE),
    Id = list_to_integer(string:substr(Line, A+1, B-1)),
    Data = string:substr(Line,B+1),
    {Id,Data}.
