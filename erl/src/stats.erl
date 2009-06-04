-module(stats).
-compile(export_all).
-include("debug.hrl").

-define(MSG_LOW_MARK,  20000).
%-define(MSG_HIGH_MARK, 200000). haven't seen behaviour requiring hi/lo mark, yet.

spawn_watcher(NamesAndPids) ->
    ProcessLoop = spawn(?MODULE,process_loop,[false]),
    spawn(?MODULE,gather_loop,[NamesAndPids, ProcessLoop]),
    ProcessLoop.

%% process_loop

process_loop(Congested) ->
    receive
	{ is_congested, Pid } ->
	    Pid ! { congested, Congested },
	    process_loop(Congested);
	{ stats, Stats } ->
	    Congested2 = congested(Stats),
	    cc_msg(Congested,Congested2),
	    process_loop(Congested2)
    end.

congested(Stats) ->
    WithoutProcessNames = [ NumMsgs || { _,NumMsgs} <- Stats ],
    TotalNumMsgs = lists:sum(lists:flatten(WithoutProcessNames)),
    TotalNumMsgs > ?MSG_LOW_MARK.

cc_msg(Congested,Congested2) ->
    case Congested of
	Congested2 -> no_change_so_no_message;
	_ -> case Congested2 of
		 true  -> d("cc on\n");
		 false -> d("cc off\n")
	     end
    end.

block_if_congested(Pid) ->
    Pid ! { is_congested, self() },
    receive 
	{ congested, Congested } ->
	    case Congested of
		true -> 
		    timer:sleep(1000),
		    block_if_congested(Pid);
		false ->
		    done
	    end
    end.

%% gather stats loop

gather_loop(NamesAndPids, ProcessLoop) ->
    Stats = [ { Name, check_msg_queues(Pids) } || { Name,Pids } <- NamesAndPids ],
    io:format("stats ~w\n",[Stats]),
    ProcessLoop ! { stats, Stats },
    timer:sleep(1000),
    gather_loop(NamesAndPids, ProcessLoop).

check_msg_queues(Pids) ->
    [ check_msg_queue(P) || P <- Pids ].
   				  
check_msg_queue(Pid) ->
    { message_queue_len, Len } = process_info(Pid, message_queue_len),
    Len.

