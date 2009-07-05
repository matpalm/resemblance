-module(shuffle).
-compile(export_all).

start() ->
    InputDirs = file_util:input_dirs(),
    io:format("InputDirs ~p\n",[InputDirs]),
    OutputDir = file_util:output_dir(),
    TmpDirNamePrefix = hd(InputDirs) ++ "_s_",

    io:format(">>>> partition\n"),
    PartitionResults = TmpDirNamePrefix ++ "1_partitioned",
    io:format("PartitionResults ~p\n",[PartitionResults]),
    put(task, partition),
    put(input_dirs, InputDirs),
    put(output_dir, PartitionResults),
    map_reduce:start(),

    io:format(">>>> sort_collate\n"),
    SortCollateResults = TmpDirNamePrefix ++ "2_sort_collate",
    put(task, sort_collate),
    put(input_dirs, [PartitionResults]),
    put(output_dir, SortCollateResults),
    map_reduce:start(),

    io:format(">>>> merge\n"),
    put(input_dirs, [SortCollateResults]),
    put(output_dir, OutputDir),
    merge:start(),

    os:cmd("rm -r " ++ PartitionResults ++ " " ++ SortCollateResults),
    
    init:stop().

