-module(stats).
-compile(export_all).

spawn_watcher(NamesAndPids) ->
    spawn(?MODULE,loop,[NamesAndPids]).

loop(NamesAndPids) ->
    Stats = [ {Name, check_msg_queues(Pids)} || { Name,Pids } <- NamesAndPids ],
    io:format("stats ~w\n",[Stats]),
    timer:sleep(1000),
    loop(NamesAndPids).

check_msg_queues(Pids) ->
    [ check_msg_queue(P) || P <- Pids ].
   				  
check_msg_queue(Pid) ->
    { message_queue_len, Len } = process_info(Pid, message_queue_len),
    Len.
