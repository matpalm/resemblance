-module(map_reduce_s).
-compile(export_all).
-include("debug.hrl").

start() ->
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
    end,
    init:stop().

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
    InFile = file_util:input_dir()++"/"++File,
    OutFile = file_util:output_dir()++"/"++File,
    Pid = spawn(?MODULE, worker, [InFile,OutFile,opts:task()]),
    Pid ! { ack, self() }.

acks(N) ->
    lists:foreach(fun(_) -> receive_an_ack() end, lists:seq(1,N)).

receive_an_ack() ->
    receive { ack, _ } -> ok end.    


worker(InFile,OutFile,Module) ->
    d("InFile=~p OutFile=~p\n",[InFile, OutFile]),
    In = bin_parser:open_file_for_read(InFile),
    Out = bin_parser:open_file_for_write(OutFile),
    InitialState = apply(Module,initial_state,[]),
    EmitFn = fun(X) -> 
		     bin_parser:write(Out,X) 
	     end,
    process(In, Module, InitialState, EmitFn),    
    file:close(In),
    file:close(Out),
    util:ack_response().

process(In, Module, State, EmitFn) ->
    Read = bin_parser:read(In),
    case Read of
	{ok,Term} ->
	    State2 = apply(Module, process, [Term, State, EmitFn]),
	    process(In, Module, State2, EmitFn);
	eof ->
	    apply(Module, finished, [State, EmitFn]);
	_ ->
	    d("other? ~p\n",[Read]),
	    Read
    end.


% common worker functions
%worker_done() ->
%    util:ack_response(). % todo: make workers use this directly

   

