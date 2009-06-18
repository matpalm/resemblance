-module(file_util).
-compile(export_all).
-include("debug.hrl").

write(File, Data) ->
    {ok,F} = file:open(File++".gz", [write,delayed_write,compressed,raw]),
    file:write(F,term_to_binary(Data)),   
%    msg("write",Data,Filename),
    file:close(F).

read(Filename) ->
    % no explicit .gz since globbed from bash shell
    {ok,F} = file:open(Filename, [read,compressed,raw]),
    Binary = slurp(F,[]),
    Data = binary_to_term(Binary),
%    msg("read",Data,Filename),
    Data.

slurp(File,Acc) ->
    Read = file:read(File,1048576),
    case Read of
	eof ->
	    list_to_binary(lists:reverse(Acc));
	{ok,B} ->
	    slurp(File,[B|Acc])
    end.  

msg(M,Data,Filename) when is_list(Data) ->
    d("~s ~p list entries to ~s\n",[M,length(Data),Filename]);

msg(M,Data,Filename) ->
    d("~s ~p dict entries to ~s\n",[M,dict:size(Data),Filename]).

cat() ->    
    Files = input_files(),
    cat(Files).

cat([]) ->
    init:stop();

cat([F|Fs]) ->
    d(format_str(),[F,read(F)]),
    cat(Fs).

format_str() ->
    FormatArg = init:get_argument(format),
    case FormatArg of
	error -> "~p ~w\n"; % no format
	_     -> "~p ~p\n"  % format
    end.
		       
	    
input_files() ->
    {ok,Args} = init:get_argument(input_files),
    hd(Args).    

output_dir() ->
    {ok,Args} = init:get_argument(output_dir),
    hd(Args).
    
