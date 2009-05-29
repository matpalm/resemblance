d(Msg) ->
    d(Msg,[]).
d(Msg,Vars) ->
    d(?MODULE, Msg, Vars).
d(Mod,Msg,Vars) ->
    io:format("~w "++atom_to_list(Mod)++" "++Msg,[self()]++Vars).
