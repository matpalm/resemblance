-module(file_util).
-compile(export_all).
-include("debug.hrl").
-define(WRITE_BUFFER_SIZE, 1048576).

open_file_for_write(Filename) ->
    {ok,F} = file:open(Filename, [write,{delayed_write,?WRITE_BUFFER_SIZE,60000},compressed]),
    F.

write(File, Terms) ->
    F = open_file_for_write(File),
    lists:foreach(fun(T) -> file:write(F,term_to_binary(T)) end, Terms),
    file:close(F).

read(Filename) -> 
    bin_parser:read_file_as_list(Filename).                                                      
    
msg(M,Data,Filename) when is_list(Data) ->
    d("~s ~p list entries to ~s\n",[M,length(Data),Filename]);

msg(M,Data,Filename) ->
    d("~s ~p dict entries to ~s\n",[M,dict:size(Data),Filename]).

cat(AArgs) ->
    Args = [ atom_to_list(A) || A <- AArgs ],
    io:format("~p\n",[Args]),
    catf(Args).

catf([]) ->
    init:stop();

catf([File|Files]) ->
    F = bin_parser:open_file(File),
    EmitFn = format_fn(File),
    cat_file(F, EmitFn, 1, bin_parser:parse(F)),
    catf(Files),
    init:stop().

cat_file(_F, _EmitFn, _N, eof) ->
    done;

cat_file(F, EmitFn, N, {ok, Term, Partial}) ->
    EmitFn(Term), % used to use N here
    cat_file(F, EmitFn, N+1, bin_parser:parse(F,Partial)).
    
format_fn(File) ->
    FormatArg = init:get_argument(format),
    FormatStr = case FormatArg of
	error -> "~p ~w\n"; % no format
	_     -> "~p ~p\n"  % format
    end,
    fun(Term) ->
	    io:format(FormatStr, [File,Term])
    end.
	    
input_files() ->
    Ls = os:cmd("ls "++input_dir()),
    Files = re:split(Ls,"\n",[{return,list}]),
    Filtered = lists:filter(
      fun(L) -> length(L) > 0 end,
      Files
      ),
    Filtered.
	       
input_dir() ->
    {ok,Args} = init:get_argument(input_dir),
    hd(hd(Args)).    

output_dir() ->
    {ok,Args} = init:get_argument(output_dir),
    hd(hd(Args)).
   
ensure_dir_created(Dir) ->
    os:cmd("mkdir "++Dir).

ensure_output_dir_created() ->
    ensure_dir_created(file_util:output_dir()).

