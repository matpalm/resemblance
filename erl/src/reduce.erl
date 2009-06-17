-module(reduce).
-compile(export_all).
-include("debug.hrl").

main() ->
    Files = file_util:files_from_command_line_args(),
    Workers = start_workers(Files),
    util:ack(Workers),
    init:stop(),
    done.

start_workers(Files) ->
    start_workers(0,Files,[]).

start_workers(_N,[],Pids) ->
    Pids;

start_workers(N,[File|Files],Pids) ->
    Pid = reducer:start(File,"sics_reduced/"++integer_to_list(N)),
    start_workers(N+1,Files,[Pid|Pids]).




   

