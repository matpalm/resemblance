-module(file_util).
-compile(export_all).
-include("debug.hrl").

write(File, Data) ->
    Filename = File ++ ".gz",
    {ok,F} = file:open(Filename, [write,delayed_write,compressed,raw]),
    d("F=~p\n",[F]),
    file:write(F,term_to_binary(Data)),
   
write_msg(Data,Filename),
    file:close(F).

write_msg(Data,Filename) when is_list(Data) ->
    d("wrote ~p list entries to ~p\n",[length(Data),Filename]);

write_msg(Data,Filename) ->
    d("wrote ~p dict entries to ~p\n",[dict:size(Data),Filename]).

read(Filename) ->
    {ok,F} = file:open(Filename, [read,compressed,raw]),
    Binary = slurp(F,[]),
    binary_to_term(Binary).

slurp(File,Acc) ->
    Read = file:read(File,1048576),
    case Read of
	eof ->
	    list_to_binary(lists:reverse(Acc));
	{ok,B} ->
	    slurp(File,[B|Acc])
    end.  

cat() ->    
    Files = files_from_command_line_args(),
    cat(Files).

cat([]) ->
    init:stop();

cat([F|Fs]) ->
    d("~p ~w\n",[F,read(F)]),
    cat(Fs).

files_from_command_line_args() ->
    {ok,Args} = init:get_argument(files),
    hd(Args).
    
    
