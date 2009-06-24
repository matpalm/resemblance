-module(map_reduce).
-compile(export_all).
-include("debug.hrl").

start() ->
    MapModule = opts:task(),
    StartFn = apply(MapModule,start_fn,[]),
    Files = file_util:input_files(),
    file_util:ensure_output_dir_created(),
    Workers = start_workers(Files,StartFn),
    %TODO: should make a max, say 4, workers and then start a new one per completion ack
    util:ack(Workers),
    init:stop().

start_workers(Files,NewWorkerFn) ->
    start_workers(Files,[],NewWorkerFn).

start_workers([],Pids,_Seeds) ->
    Pids;

start_workers([File|Files],Pids,NewWorkerFn) ->
    InFile = file_util:input_dir()++"/"++File, 
    OutFile = file_util:output_dir()++"/"++File,
io:format("worker infile ~p outfile ~p\n",[InFile,OutFile]),
    Pid = NewWorkerFn(InFile,OutFile),
    start_workers(Files,[Pid|Pids],NewWorkerFn).

% common worker functions
worker_done() ->
    util:ack_response(). % todo: make workers use this directly

   

