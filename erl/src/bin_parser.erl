-module(bin_parser).
%-compile(export_all).
-export([read_file_as_list/1, open_file/1, parse/1, parse/2, parse_binary/1]).
-define(BUFFER_READ_SIZE, 4096).

%-----------------------
% public api

% open file handle for passing to parse_file 
open_file(Filename) ->
    {ok,F} = file:open(Filename,[read,binary,raw,compressed]),
    F.

% parse
% returns either
%  eof  
%  { ok, Term, ContinuationData }
%  { error, <reason> }

parse(F) ->
    parse_file(<<>>, <<>>, F).

parse(F, PartialData) ->
    parse_file(<<>>, PartialData, F).

% simple helper to collect entire file as list
read_file_as_list(Filename) ->
    F = open_file(Filename),
    slurp(F, <<>>, []).

slurp(F, PartialData, Acc) -> 
    Next = parse(F, PartialData),
    case Next of
	eof -> lists:reverse(Acc);
	{ ok, Term, Continuation } -> slurp(F, Continuation, [Term|Acc]);
	Other -> Other
    end.	     


%-----------------------
% internals

parse_file(<<>>, <<>>, F) ->
    Read = file:read(F, ?BUFFER_READ_SIZE),
    case Read of
	{ok, Data} -> parse_file(<<>>, Data, F);
	eof ->        eof
    end;

parse_file(<<>>, Data, F) ->
    Result = parse_binary(Data),
    case Result of
	{ ok, Term, Data2 } -> { ok, Term, Data2 };
	{ partial, Binary } -> parse_file(Binary, <<>>, F)
    end;

parse_file(PartialData, <<>>, F) ->
    Read = file:read(F, ?BUFFER_READ_SIZE),
    case Read of
	{ok, Data} -> parse_file(<<>>, <<PartialData/binary, Data/binary>>, F);
	eof ->        { error, file_ended_with_partial_binary, PartialData }
    end.

	     
parse_binary(Binary) ->
    Result = start_seperator(Binary),
    {ParseResult, TermBinary, Data} = Result,
    case ParseResult of
	ok      -> { ok, binary_to_term(TermBinary), Data};
	partial -> { partial, Binary };
	_       -> Result
    end.


start_seperator(<<>>) ->
    init:stop();

start_seperator(<<131>>) ->	     
    { partial, nil, <<131>> };

start_seperator(<<131, Tag, Data/binary>>) ->
    Parsed = start_term(Tag, Data),
    { Result, Term, Data2 } = Parsed,
    case Result of 
	ok      -> { ok, <<131, Term/binary>>, Data2 };
	partial -> { partial, nil, nil };
	_       -> Parsed
    end;

start_seperator(<<First, Rest/binary>>) ->
    io:format("ERR: expected start of new term, got ~w, try resync\n",[First]),
    parse(Rest).


start_term(97,Data) -> parse_small_integer(Data);
start_term(98,Data) -> parse_integer(Data);
start_term(100,Data) -> parse_atom(Data);
start_term(104,Data) -> parse_small_tuple(Data);
start_term(106,Data) -> {ok, <<106>>, Data}; % empty list
start_term(107,Data) -> parse_string(Data);
start_term(108,Data) -> parse_list(Data);
start_term(Tag, Data) -> { error, {unknown_type, Tag}, Data}.


parse_small_integer(<<IntVal, Data/binary>>) ->
    { ok, <<97,IntVal>>, Data};

parse_small_integer(_Data) ->
    { partial, nil, nil }.


parse_integer(<<B1,B2,B3,B4, Data/binary>>) ->
    { ok, <<98,B1,B2,B3,B4>>, Data };

parse_integer(_Data) ->
    { partial, nil, nil }.


parse_atom(<<0, Len, Data/binary>>) ->
    % spec implies (and tests show) first byte of atom length is always 0 ??
    AccSeed = <<100,0,Len>>, % first bytes of atom
    accum_binary(Len, Data, AccSeed);

parse_atom(_Data) ->
    { partial, nil, nil }.


parse_small_tuple(<<Arity, Data/binary>>) ->
    Parsed = parse_elements(Arity, Data, [<<Arity>>]),
%    io:format("G ~w\n",[Parsed]),
    {Result, Elements, Data2 } = Parsed,
    case Result of 
	ok      -> { ok, <<104, Elements/binary>>, Data2 };
	partial -> { partial, nil, nil };
	_       -> Parsed
    end;

parse_small_tuple(_Data) ->
    { partial, nil, nil }.


parse_string(<<0, Len, Data/binary>>) ->
    AccSeed = <<107,0,Len>>,
    accum_binary(Len, Data, AccSeed);

parse_string(_Data) ->
    { partial, nil, nil }.


parse_list(<<L1,L2,L3,L4, Data/binary>>) ->
    Len = L1 * (256*256*256) + L2 * (256*256) + L3 * 256 + L4,
    Parsed = parse_elements(Len, Data, [<<L1,L2,L3,L4>>]),
    {Result, Elements, Data2 } = Parsed,
    case Result of
	ok ->
	    << 106, Data3/binary >> = Data2, % ignore improper lists?? see spec
	    { ok, <<108, Elements/binary, 106>>, Data3 };
	partial ->
	    { partial, nil, nil };
	_ ->
	    Parsed
    end;

parse_list(_Data) ->
    { partial, nil, nil }.



accum_binary(0, Data, Acc) ->
    { ok, Acc, Data };

accum_binary(_Len, <<>>, _Acc) ->
    { partial, nil, nil };
 
accum_binary(Len, <<B, Data/binary>>, Acc) -> % todo: might be faster to accum to a list and call list_to_binary at end?
    accum_binary(Len-1, Data, <<Acc/binary, B>>).


parse_elements(0, Data, Acc) ->
    { ok, list_to_binary(lists:reverse(Acc)), Data };

parse_elements(_N, <<>>, _Acc) ->
    { partial, nil, nil };

parse_elements(N, <<Tag, Data/binary>>, Acc) ->
    Parsed = start_term(Tag, Data),
%io:format("F ~p\n",[Parsed]),
    { Result, ElementData, Data2 } = Parsed,
     case Result of 
	ok      -> parse_elements(N-1, Data2, [ElementData|Acc]);
	partial -> { partial, nil, nil };
	_       -> Parsed
    end.
		   
    
    
