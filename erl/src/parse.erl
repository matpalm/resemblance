-module(parse).
-compile(export_all).

line(Line) ->
    {ok,RE} = re:compile("^\\d+"),
    {match,[{A,B}]} = re:run(Line,RE),
    Id = list_to_integer(string:substr(Line,A+1,B)),
    Data = string:substr(Line,B+2),
    { Id, Data }.
