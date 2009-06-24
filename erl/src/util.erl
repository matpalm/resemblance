-module(util).
-export([ shingles/1, shingles/2, uhash/2, uhash_seed/1, merge/2, 
	  ack/1, ack_response/0,
	  distribute_over_N_lists/2, distribute_over_N_files/2,
	  slurp_stdin/0, slurp_stdin/1]).

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

%remove_most_freq(ListOfLists) ->
%    MostFrequent = sets:from_list(calc_most_freq(ListOfLists)),
%    [ remove_most_freq_set(List, MostFrequent) || List <- ListOfLists ].

%remove_most_freq_set(List, MostFreqSet) ->
%    sets:to_list(sets:subtract(sets:from_list(List), MostFreqSet)).

%calc_most_freq(ListOfLists) ->
%    % [ [a,b,c], [a,b,e], [b,e,g] ]
%    Flattened = lists:flatten(ListOfLists),
%    % [ a,b,c,a,b,e,b,e,g ]
%    Freqs = lists:foldl(
%	      fun(E,Freq) -> dict:update_counter(E,1,Freq) end,
%	      dict:new(),
%	      Flattened
%	     ),    
%    FreqList = lists:reverse(lists:keysort(2,dict:to_list(Freqs))),
%    % [ {b,3},{a,2},{e,2},{c,1},{g,1} ]
%    Total = lists:sum([Count || {Term,Count} <- FreqList ]),
%    Cutoff = Total * 0.05,
%    io:format("T=~p C=~p\n",[Total,Cutoff]),
%    io:format("FreqList ~w\n",[FreqList]),
%    MostFreq = collect_head_until_cutoff(FreqList, Cutoff),
%    io:format("MostFreq ~w\n",[MostFreq]),
%    MostFreq.

%collect_head_until_cutoff(List, Cutoff) ->
%    collect_head_until_cutoff(List, Cutoff, [], nil).

%collect_head_until_cutoff([{Term,Freq}|T]=List, Cutoff, Acc, LastRemoved) ->
%    case Freq < Cutoff of
%	true ->  collect_head_until_cutoff(T, Cutoff - Freq, [Term|Acc], Freq);
%	false -> collect_head_while_equals(List, LastRemoved, Acc)
%    end.

%collect_head_while_equals([{Term,Freq}|T]=List, FreqToRemove, Acc) ->
%    case Freq == FreqToRemove of
%	true  -> collect_head_while_equals(T,Freq,[Term|Acc]);
%	false -> Acc
%    end.
	     
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
     
distribute_over_N_files(List,N) ->
    write_to_file(0,distribute_over_N_lists(List,N)).

write_to_file(_N,[]) ->
    done;

write_to_file(N, [H|T]) ->
    file_util:write(file_util:output_dir()++"/"++integer_to_list(N)++".gz", H),
    write_to_file(N+1,T).

distribute_over_N_lists(List,N) ->
    EmptyLists = lists:duplicate(N,[]),
    distribute_over_N_lists(List,EmptyLists,[]).

distribute_over_N_lists([],ToFill,Filled) ->
    ToFill ++ Filled;

distribute_over_N_lists(List,[],Filled) ->
    distribute_over_N_lists(List,Filled,[]);

distribute_over_N_lists([H|T],[LH|LT],Filled) ->
    distribute_over_N_lists(T, LT, [[H|LH]|Filled]).

