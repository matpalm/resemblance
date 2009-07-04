-module(opts).
-compile(export_all).

override_prop(Key,Value) ->
    put(Key,Value). 

num_workers() ->
    int_prop(num_workers,4).

num_files() ->
    int_prop(num_files,10).

num_partitions() ->
    int_prop(num_partitions,10).

task() ->
    atom_prop(task).

%num_reducers() ->
%    int_prop(num_reducers,4).

%write_to_disk_freq() ->
%    int_prop(write_to_disk_freq,3000).

%cc_check_freq() ->
%    int_prop(cc_check_freq,1000).

sketch_size() ->
    int_prop(sketch_size,20).

%sketches_in_common_cutoff() ->
%    int_prop(sketches_in_common_cutoff, (sketch_size()/3)).

shingle_size() ->
    int_prop(shingle_size,10).


int_prop(Flag, Dft) ->
    prop(Flag, fun(X) -> list_to_integer(X) end, Dft).

atom_prop(Flag) ->
    prop(Flag, fun(X) -> list_to_atom(X) end).    

string_prop(Flag) ->
    prop(Flag, fun(X) -> X end).

string_prop(Flag, Dft) ->
    prop(Flag, fun(X) -> X end, Dft).
    
prop(Flag, F) ->
    prop(Flag, F, no_default).

prop(Flag, F, Dft) ->
    case get(Flag) of
	undefined ->
	    Value = init:get_argument(Flag),
	    case Value of
		error -> 
		    case Dft of
			no_default -> {no_command_line_set_for,Flag,and_no_dft_given,Value};
			_ -> Dft
		    end;
		{ok,V} -> 
		    F(hd(hd(V)))
	    end;
	Val -> 
	    Val
    end.
    

	     
