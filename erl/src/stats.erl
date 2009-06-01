-module(stats).
-compile(export_all).

spawn_watcher(Pids,Names) ->
    spawn(?MODULE,loop,[Pids,Names]).

loop(Pids,Names) ->
    io:format("stats\n"),
    lists:foreach(
      fun({P,N}) -> check_msg_queue(P,N) end,
      lists:zip(Pids,Names)
      ),
    timer:sleep(1000),
    loop(Pids,Names).

check_msg_queue(Pid,Name) ->
    { message_queue_len, N } = process_info(Pid, message_queue_len),
    io:format("~p ~p\n",[Name,N]).
