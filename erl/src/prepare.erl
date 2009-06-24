-module(prepare).
-compile(export_all).

main() ->
    file_util:ensure_output_dir_created(),
    NumFiles = opts:num_mappers(),
    ParseFn = fun(L) -> parse_line(L) end,
    Data = util:slurp_stdin(ParseFn),
    util:distribute_over_N_files(Data,NumFiles),
    init:stop().

parse_line(Line) ->
    {ok,RE} = re:compile("^.*? "),
    {match,[{A,B}]} = re:run(Line,RE),
    Id = list_to_integer(string:substr(Line, A+1, B-1)),
    Data = string:substr(Line,B+1),
    {Id,Data}.
