d(Msg) ->
    d(Msg,[]).
d(Msg,Vars) ->
    d(?MODULE, Msg, Vars).
d(Mod,Msg,Vars) ->
    io:fwrite("~s ~w "++atom_to_list(Mod)++" "++Msg,[util:timestamp(),self()]++Vars).
