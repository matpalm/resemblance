-module(shuffle).
-compile(export_all).

start() ->
    InputDir = file_util:input_dir(),
    OutputDir = file_util:output_dir(),

    PartitionResults = InputDir ++ "_1_partitioned",
    put(task,partition),
    put(input_dir, InputDir),
    put(output_dir, PartitionResults),
    map_reduce:start(),

    SortCollateResults = InputDir ++ "_2_sort_collate",
    put(task, sort_collate),
    put(input_dir, PartitionResults),
    put(output_dir, SortCollateResults),
    map_reduce_s:start(),

    put(input_dir, SortCollateResults),
    put(output_dir, OutputDir),
    merge:start(),

    init:stop().

