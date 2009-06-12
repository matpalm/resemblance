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
  
% given  ["1.0.gz","1.1.gz","2.0.gz","2.1.gz","2.2.gz"] 
% return [ {"0",["1.0.gz","2.0.gz"]} , {"1",["1.1.gz","2.1.gz"]}, {2,["2.2.gz"]} ] 
partition_filenames(Filenames) ->
    FilesSplit = [ split(Filename) || Filename <- Filenames ],
    lists:keysort(1,partition(FilesSplit)).
    
split(Filename) ->
    Bits = re:split(Filename,"\\."),
    [ binary_to_list(S) || S <- Bits ].

partition(Files) ->
    partition(Files, dict:new()).

partition([],Partitions) ->
    dict:to_list(Partitions);

partition([[_,B,_]=File|Files],Partitions) ->
    [A,B,C] = File,
    Filename = A ++ "." ++ B ++ "." ++ C,
    case dict:is_key(B,Partitions) of
	true ->  partition(Files,dict:append(B,Filename,Partitions));
	false -> partition(Files,dict:store(B,[Filename],Partitions))
    end.
    
    
    
    
