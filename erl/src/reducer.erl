-module(reducer).
-compile(export_all).

start(InputFile,OutputFile) ->
    spawn(?MODULE,reduce,[InputFile,OutputFile]).

reduce(InputFile,OutputFile) ->
    KVList = file_util:read(InputFile),
    Result = process(KVList),
    % filter?
    file_util:write(OutputFile,Result),
    receive
	{ack,Pid} ->
	    Pid ! {ack,self()}
    end.

process(KVList) ->
    process(KVList,[]).

process([],Acc) ->
    Acc;

process([{_K,VList}|T], Acc) ->
    Acc2 = combos(VList,Acc),
    process(T,Acc2).

combos([],Acc) ->
    Acc;

combos([H|T],Acc) ->
    Pairs = all_pairs(H,T,[]),
    combos(T,lists:append(Pairs,Acc)).

all_pairs(_E,[],Acc) ->
    Acc;

all_pairs(E,[H|T],Acc) ->
    all_pairs(E,T,[{E,H}|Acc]).

