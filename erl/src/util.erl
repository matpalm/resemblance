-module(util).
-export([ shingles/1, shingles/2, uhash/2, uhash_seed/1, merge/2, 
	  ack/1, ack_response/0,
	  slurp_stdin/0, slurp_stdin/1,
	  timestamp/0]).

% todo get rid of most of these above functions, who is calling them??

%-define(UHASH_M,   1234). % largish prime < MAX
%-define(UHASH_MAX, 23424). % hash max value (2^32)

-define(UHASH_M,   2305843009213693951).  % largish prime (2^61)-1 < MAX
-define(UHASH_MAX, 18446744073709551616). % hash max value (2^64)

shingles(Str) ->
    shingles(Str, opts:shingle_size()).

shingles(_S,N) when N < 1 ->
    [];
shingles(S,N) when N > length(S) ->
    [];
shingles(S,N) ->
    unique_elems([ string:substr(S,Offset,N) || Offset <- lists:seq(1,length(S)-N+1) ]).

unique_elems(L) ->
    sets:to_list(sets:from_list(L)).
    
uhash(Str, A) ->
    uhash(Str, A, 0).
uhash([], _A, V) ->
    V rem ?UHASH_MAX;
uhash([S|TS], [A|TA], V) ->
    SHash = (S*A) rem ?UHASH_M,
    V2 = V + SHash, 
    uhash(TS,TA,V2).

uhash_seed(N) ->    
    [ random:uniform(?UHASH_M) || _ <- lists:seq(1,N) ].

ack(Pid) when is_atom(Pid) -> 
    ack(get(Pid));
ack(Pids) when is_list(Pids) -> 
    [ ack(P) || P <- Pids];
ack(Pid) ->
    Pid ! { ack, self() },
    receive { ack, Pid } -> ok end.    

ack_response() ->
    receive
	{ack,Pid} ->
	    Pid ! {ack,self()}
    end.

merge(L1,L2) ->
    merge(L1,L2,[]).

merge([],[],Acc) ->
    lists:reverse(Acc);

merge([H|T],[],Acc) ->
    merge(T,[],[H|Acc]);

merge([],L,Acc) ->
    merge(L,[],Acc);

merge([{K1,V1}=H1|T1]=L1,[{K2,V2}=H2|T2]=L2,Acc) ->
    case K1==K2 of
	true ->	    
	    merge(T1,T2,[{K1,lists:merge(V1,V2)}|Acc]);
	false ->
	    case K1 < K2 of
		true  -> merge(T1,L2,[H1|Acc]);
		false -> merge(L1,T2,[H2|Acc])
	    end
    end.    

slurp_stdin() ->    
    IdentityFn = fun(L) -> L end,
    slurp_stdin(IdentityFn).

slurp_stdin(Fn) ->
    slurp_stdin(Fn,[]).

slurp_stdin(Fn,Acc) ->
    case io:get_line('') of 
	eof ->  lists:reverse(Acc);
	Line -> slurp_stdin(Fn,[Fn(chomp(Line))|Acc])
    end.
	
chomp(S) -> 
    string:substr(S,1,length(S)-1).
     
timestamp() ->		  
    iso_8601_fmt(erlang:localtime()).

iso_8601_fmt(DateTime) ->
    % http://schemecookbook.org/Erlang/TimeISO8601
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B~2.10.0B~2.10.0B ~2.10.0B~2.10.0B~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

