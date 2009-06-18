-module(reducer).
-export([start_fn/0,reduce/2]).
%-compile(export_all).

start_fn() ->
    NewWorkerFn = 
	fun(InFile,OutFile) ->
		spawn(?MODULE,reduce,[InFile,OutFile])
	end,
    NewWorkerFn.

reduce(InputFile,OutputFile) ->
    KVList = file_util:read(InputFile),
    Result = process(KVList),
    ResultWithFreqOne = [ {KV,1} || KV <- Result ],
    % filter?
    file_util:write(OutputFile,ResultWithFreqOne),
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
    all_pairs(E,T,[in_order({E,H})|Acc]).

in_order({A,B}) ->
    case A < B of
	true  -> {A,B};
	false -> {B,A}
    end.
	     

