-module(sketches_in_common).
-compile(export_all).
-include("debug.hrl").

start() ->
    spawn(?MODULE,loop,[dict:new()]).

loop(Freq) ->
    receive
	dump ->
	    d("freqs=~w\n",[lists:sublist(lists:reverse(lists:keysort(2,dict:to_list(Freq))),10)]),
	    loop(Freq);

	{ sketch_in_common, Id1, Id2 } ->
	    Key = ensure_first_less_than(Id1,Id2),
	    Freq2 = dict:update_counter(Key, 1, Freq),
	    loop(Freq2)

    after 15000 ->
	    d("timeout\n")
    end.

ensure_first_less_than(A,B) when A > B ->
    ensure_first_less_than(B,A);

ensure_first_less_than(A,B) ->
    { A,B }.
    

