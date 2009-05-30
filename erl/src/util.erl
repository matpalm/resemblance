-module(util).
-compile(export_all).
-include("consts.hrl").

shingles(Str) ->
    shingles(Str, ?SHINGLE_SIZE).

shingles(_S,N) when N < 1 ->
    [];
shingles(S,N) when N > length(S) ->
    [];
shingles(S,N) ->
    [ string:substr(S,Offset,N) || Offset <- lists:seq(1,length(S)-N+1) ].

uhash(Str, A) ->
%    io:format("processing ~p\n",[Str]),
    uhash(Str, A, 0).
uhash([], _A, V) ->
    R = V rem ?UHASH_MAX,
%    io:format("R=~w V=~w\n",[R,V]),
    R;
uhash([S|TS], [A|TA], V) ->
    SHash = (S*A) rem ?UHASH_M,
    V2 = V + SHash, 
%    io:format("V=~w S=~w A=~w SHash=~w V2=~w\n",[V,S,A,SHash,V2]),
    uhash(TS,TA,V2).

uhash_seed(N) ->    
    [ random:uniform(?UHASH_M) || _ <- lists:seq(1,N) ].


tostr() ->
    spawn(?MODULE,tostrloop,[]).

tostrloop() ->
    receive
	M ->
	    io:format("~w tostr ~p\n",[self(),M])
    after 15000 ->
	    io:format("~w tostrloop timeout\n",[self()]),
	    exit(0)
    end,
    tostrloop().
	    


