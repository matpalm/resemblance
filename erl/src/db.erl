-module(db).
-compile(export_all).

start() ->
    mysql:start_link(p1, "localhost", "mat", "mat", "resemblance").
    
insert_document_fn() ->
    mysql:prepare(insert_document, <<"insert into documents (id,data) values (?,?)">>),
    fun(Id,Data) ->
	    mysql:execute(p1, insert_document, [Id,Data])
    end.

insert_sketch_in_common_fn() ->
    mysql:prepare(update_freq, <<"insert into sketches_in_common (id1,id2,count) values (?,?,1) on duplicate key update count=count+1">>),
    fun(Id1,Id2) ->
	    mysql:execute(p1, update_freq, [Id1,Id2])
    end.

add_to_db_pool() ->
    mysql:connect(p1, "localhost", undefined, "mat", "mat", "resemblance", true).    

