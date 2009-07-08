-module(prepare_id_nums).
-compile(export_all).

parse_line(Line) ->
    Split = re:split(Line," "),
    [Key|Values] = [ list_to_integer(binary_to_list(X)) || X <- Split ],
    {Key,Values}.


