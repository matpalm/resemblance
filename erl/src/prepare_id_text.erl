-module(prepare_id_text).
-compile(export_all).

parse_line(Line) ->
    {ok,RE} = re:compile("^.*? "),
    {match,[{A,B}]} = re:run(Line,RE),
    Id = list_to_integer(string:substr(Line, A+1, B-1)),
    Data = string:substr(Line,B+1),
    {Id,Data}.
