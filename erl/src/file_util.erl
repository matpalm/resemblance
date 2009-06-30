-module(file_util).
-compile(export_all).
-include("debug.hrl").
-define(WRITE_BUFFER_SIZE, 1048576).

cat(AArgs) ->
    Args = [ atom_to_list(A) || A <- AArgs ],
    catf(Args).

read_all_from_file(Filename) ->
    F = bin_parser:open_file_for_read(Filename),
    read_all(F,[]).

read_all(F,Acc) ->
    R = bin_parser:read(F),
    case R of
	{ok,T} -> read_all(F,[T|Acc]);
	eof    -> lists:reverse(Acc)
    end.    

catf([]) ->
    init:stop();

catf([File|Files]) ->
    F = bin_parser:open_file_for_read(File),
    EmitFn = format_fn(File),
    cat_file(F, EmitFn, 1, bin_parser:read(F)),
    catf(Files),
    init:stop().

cat_file(_F, _EmitFn, _N, eof) ->
    done;

cat_file(F, EmitFn, N, {ok,Term}) ->
    EmitFn(Term), % used to use N here
    cat_file(F, EmitFn, N+1, bin_parser:read(F)).
    
format_fn(File) ->
    FormatArg = init:get_argument(format),
    FormatStr = case FormatArg of
	error -> "~p ~w\n"; % no format
	_     -> "~p ~p\n"  % format
    end,
    fun(Term) ->
	    io:format(FormatStr, [File,Term])
    end.
	   
% input files doesnt include the input dir so that files
% can be made in the output dir with the same filenames 
input_files() ->
    Ls = os:cmd("ls "++input_dir()),
    Files = re:split(Ls,"\n",[{return,list}]),
    Filtered = lists:filter(
      fun(L) -> length(L) > 0 end,
      Files
      ),
    Filtered.
	       
input_dir() ->
    opts:string_prop(input_dir).

output_dir() ->
    opts:string_prop(output_dir).
   
    
ensure_dir_created(Dir) ->
    os:cmd("mkdir "++Dir).

ensure_output_dir_created() ->
    ensure_dir_created(file_util:output_dir()).

