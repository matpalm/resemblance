-module(map_reduce_s).
-compile(export_all).
-include("debug.hrl").

start() ->
    put(next_file_num, 0),
    Files = file_util:input_files(),
    d("Files ~p\n",[Files]),
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
    end,
    init:stop().

start_workers(Files) ->
    lists:foreach(fun(F) -> start_worker(F) end, Files).

start_worker_for_each_completion([]) ->
    done;

start_worker_for_each_completion([File|Files]) ->
    receive_an_ack(),
    start_worker(File),
    start_worker_for_each_completion(Files).

start_worker(InFile) ->
    OutFile = file_util:output_dir()++"/"++integer_to_list(next_file_num()),
    Pid = spawn(?MODULE, worker, [InFile,OutFile,opts:tasks()]),
    Pid ! { ack, self() }.

acks(N) ->
    lists:foreach(fun(_) -> receive_an_ack() end, lists:seq(1,N)).

receive_an_ack() ->
    receive { ack, _ } -> ok end.    


worker(InFile,OutFile,Tasks) ->

    d("InFile=~p OutFile=~p Tasks=~p\n",[InFile, OutFile, Tasks]),

    In = bin_parser:open_file_for_read(InFile),
    Out = bin_parser:open_file_for_write(OutFile),

    Params = [ apply(Task,params,[]) || Task <- Tasks ],
    FinalEmitFn = fun(X) -> bin_parser:write(Out,X) end,
    ProcessFn = wire_up(lists:reverse(Tasks), lists:reverse(Params), FinalEmitFn),
    process(In, ProcessFn),

    file:close(In),
    file:close(Out),
    util:ack_response().

wire_up([],_,FirstProcessFn) ->
    FirstProcessFn;

wire_up([T|Tasks], [P|Params], EmitFn) ->
    PreceedingEmitFn = fun(X) -> apply(T, process, [X, P, EmitFn]) end,
    wire_up(Tasks, Params, PreceedingEmitFn).		  
    
process(In, ProcessFn) ->
    Read = bin_parser:read(In),
    case Read of
	{ok,Term} ->
	    ProcessFn(Term),
	    process(In, ProcessFn);
	eof ->
	    done;
	_ ->
	    d("other? ~p\n",[Read]),
	    Read
    end.


next_file_num() ->
    N = get(next_file_num),
    put(next_file_num, N+1),
    N.

% common worker functions
%worker_done() ->
%    util:ack_response(). % todo: make workers use this directly

   

