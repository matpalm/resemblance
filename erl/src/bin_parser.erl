-module(bin_parser).
-export([open_file_for_read/1, open_file_for_write/1, write/2, read/1]).
-define(WRITE_BUFFER_SIZE, 1048576).

%-----------------------
% public api

% open file handle for passing to parse_file 
open_file_for_read(Filename) ->
    case file:open(Filename,[read,binary,raw,compressed]) of
	{ ok,F } -> F;
	E -> io:format("error opening ~p (?) ~p\n",[Filename,E])
    end.

open_file_for_write(Filename) ->
    {ok,F} = file:open(Filename, [write,{delayed_write,?WRITE_BUFFER_SIZE,60000},raw,compressed]),
    F.

write(F, Term) ->
    TermBinary = term_to_binary(Term),    
    Size = size(TermBinary),
    SizeBytes = size_to_bytes(Size),
    file:write(F, [ <<19,75>>, SizeBytes, TermBinary ]).

% eof
% { ok, Term }
% { err, ErrorInfo }
read(F) ->
    case file:read(F,6) of 
	{ok,Bytes} -> {ok,read_term(F,Bytes)};
	EofOrErr -> EofOrErr 
    end.

read_term(F, <<19,75,LengthBytes/binary>>) ->
    Length = bytes_to_size(LengthBytes),
    case file:read(F, Length) of 
	{ok,Bytes} -> binary_to_term(Bytes);
	Other -> { error, {expected_term_bytes_got, Other}}
    end;

read_term(_F, Data) ->
    {error,{expected_header_but_got, Data}}.

size_to_bytes(Size) ->		   
    % todo: look up bit shifting operators when off train
    F1 = 256*256*256,
    L1 = Size div F1,
    Size2 = Size rem F1, 
    F2 = 256*256,
    L2 = Size2 div F2,
    Size3 = Size2 rem F2,
    F3 = 256,
    L3 = Size3 div F3,
    Size4 = Size3 rem F3,
    << L1, L2, L3, Size4 >>.

bytes_to_size(<<L1,L2,L3,L4>>) ->    
    L1 * (256*256*256) + L2 * (256*256) + L3 * 256 + L4.

