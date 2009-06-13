-module(sketches).
-compile(export_all).
-include("debug.hrl").

write(File, List) ->
    Filename = "sics/" ++ File ++ ".gz",
    {ok,F} = file:open(Filename, [write,delayed_write,compressed,raw]),
    d(">> wrote ~p entries to ~p\n",[length(List),Filename]),
    file:write(F,term_to_binary(List)),
    d("<< wrote ~p entries to ~p\n",[length(List),Filename]),
    file:close(F).

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
  
number_of_entries(Filename) ->
    length(read(Filename).)
    
    
    
    
