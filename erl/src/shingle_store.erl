-module(shingle_store).
-compile(export_all).
-include("debug.hrl").

start() ->
    spawn(?MODULE,loop,[dict:new()]).

loop(Store) ->
    receive
	{ ack, Pid } ->
	    Pid ! { ack, self() },
	    loop(Store);

	{ Id, {shingles,Shingles}} ->
	    loop(dict:store(Id,Shingles,Store));

	{ jaccard_coeff_for, Id1, Id2, NumCommon } ->
	    L1 = dict:fetch(Id1,Store),
	    L2 = dict:fetch(Id2,Store),
%	    io:format("~p ~p ~p\n",[Id1,Id2,coeff:jaccard(L1,L2)]),
	    io:format("~p ~p ~p ~p\n",[Id1,Id2,coeff:jaccard(L1,L2),NumCommon]),
	    loop(Store);

	dump ->
	    d("~p\n",[dict:to_list(Store)]),
	    loop(Store);

	M ->
	    d("unexpected ~w\n",[M]),
	    loop(Store)
    end.
	    
