-module(opts).
-compile(export_all).

num_mappers() ->
    int_prop(num_mappers,6).

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
    Value = init:get_argument(Flag),
    case Value of
	error -> Dft;
        {ok,V} -> list_to_integer(hd(hd(V)))
    end.

atom_prop(Flag) ->		       
    Value = init:get_argument(Flag),
    case Value of
        {ok,V} -> list_to_atom(hd(hd(V)));
	_ -> Value
    end.
    
	     
