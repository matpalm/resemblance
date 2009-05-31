-module(util).
-export([
	 shingles/1, shingles/2,
	 uhash/2, uhash_seed/1,
	 tostr/0, tostrloop/0,
	 ack/1
	]).
	  
-include("consts.hrl").

shingles(Str) ->
    shingles(Str, ?SHINGLE_SIZE).

shingles(_S,N) when N < 1 ->
    [];
shingles(S,N) when N > length(S) ->
    [];
shingles(S,N) ->
    sets:to_list(sets:from_list([ string:substr(S,Offset,N) || Offset <- lists:seq(1,length(S)-N+1) ])).

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

ack(Pid) when is_atom(Pid) -> 
    ack(get(Pid));
ack(Pids) when is_list(Pids) -> 
    [ ack(P) || P <- Pids];
ack(Pid) ->
    Pid ! { ack, self() },
    receive { ack, Pid } -> ok end.    
