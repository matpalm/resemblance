-module(map_reduce).
-compile(export_all).
-include("debug.hrl").

start() ->
    MapModule = opts:task(),
    put(start_fn, apply(MapModule,start_fn,[])),
    Files = file_util:input_files(),
    NumWorkers = opts:num_workers(),
    file_util:ensure_output_dir_created(),
    case length(Files) < NumWorkers of
	true ->
	    start_workers(Files),     
	    acks(length(Files));
	false ->
	    { Initial, Remaining } = lists:split(NumWorkers, Files),
	    start_workers(Initial),
	    start_worker_for_each_completion(Remaining),
	    acks(NumWorkers)
    end.

start_workers(Files) ->
    d("start for files ~p\n",[Files]),
    lists:foreach(fun(F) -> start_worker(F) end, Files).

start_worker_for_each_completion([]) ->
    done;

start_worker_for_each_completion([File|Files]) ->
    d("start for file ~p, num files remaining ~p\n",[File,length(Files)]),
    receive_an_ack(),
    start_worker(File),
    start_worker_for_each_completion(Files).

start_worker(File) ->
    d("start worker ~p\n",[File]),
    InFile = file_util:input_dir()++"/"++File, 
    OutFile = file_util:output_dir()++"/"++File,
    NewWorkerFn = get(start_fn),
    Pid = NewWorkerFn(InFile, OutFile),
    Pid ! { ack, self() }.

acks(N) ->
    lists:foreach(fun(_) -> receive_an_ack() end, lists:seq(1,N)).

receive_an_ack() ->
    receive { ack, _ } -> ok end.    

% common worker functions
worker_done() ->
    util:ack_response(). % todo: make workers use this directly

   

