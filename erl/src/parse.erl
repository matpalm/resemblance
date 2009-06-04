-module(parse).
-export([each_line/2,parse_line/1]).
-define(BUFFER_SIZE,1024).

each_line(Filename, EmitFn) ->
    { ok, Handle } = file:open(Filename, [read,raw,binary]),
    process([], read_chunk(Handle), Handle, EmitFn, 1).

read_chunk(Handle) ->
    Read = file:read(Handle,?BUFFER_SIZE),
    case Read of
        eof -> [];
        {ok,Data} -> Data
    end.

process(Collected, <<>>, Handle, EmitFn, N) ->
    NextFromFile = read_chunk(Handle),
    case NextFromFile of 
	[] -> final_emit(Collected, EmitFn, N);
	Buff -> process(Collected, Buff, Handle, EmitFn, N)
    end;

% hit a newline, emit collected
process(Collected, <<"\n",Rest/binary>>, Handle, EmitFn, N) ->
    EmitFn(convert_binary(Collected), N),
    process([], Rest, Handle, EmitFn, N+1);

% collecting data
process(Collected, <<C:1/binary,Rest/binary>>, Handle, EmitFn, N) ->
    process([C|Collected], Rest, Handle, EmitFn, N).

final_emit(Collected, EmitFn, N) ->
    case length(Collected) > 0 of
	true -> EmitFn(convert_binary(Collected), N);
	false -> done
    end.

convert_binary(Binary) ->
    binary_to_list(list_to_binary(lists:reverse(Binary))). % boomshanka voodoo

parse_line(Line) ->
    {ok,RE} = re:compile("^.*? "),
    {match,[{A,B}]} = re:run(Line,RE),
    Id = list_to_integer(string:substr(Line, A+1, B-1)),
    Data = string:substr(Line,B+1),
    {Id,Data}.
    
