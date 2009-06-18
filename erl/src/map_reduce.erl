-module(map_reduce).
-compile(export_all).
-include("debug.hrl").

main() ->
    io:format("ERROR, pass single arg which is module of map or reduce\n"),   
    init:stop().

main(Args) ->
    MapModule = hd(Args),
    StartFn = apply(MapModule,start_fn,[]),
    Files = file_util:input_files(),
    util:ensure_output_dir_created(),
    Workers = start_workers(Files,StartFn),
    %TODO: should make a max, say 4, workers and then start a new one per completion ack
    util:ack(Workers),
    init:stop(),
    done.

start_workers(Files,NewWorkerFn) ->
    start_workers(0,Files,[],NewWorkerFn).

start_workers(_N,[],Pids,_Seeds) ->
    Pids;

start_workers(N,[File|Files],Pids,NewWorkerFn) ->
    Outfile = file_util:output_dir()++"/"++integer_to_list(N),
    Pid = NewWorkerFn(File,Outfile),
    start_workers(N+1,Files,[Pid|Pids],NewWorkerFn).


   

