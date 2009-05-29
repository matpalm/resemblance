-module(rr_router).
-compile(export_all).
-include("debug.hrl").

start(Targets) ->
    spawn(?MODULE,loop,[Targets]).

loop(Targets) ->
%    d(">loop across ~w\n",[Targets]),
    receive
	Msg ->
%	    d("forwarding ~w\n",[Msg]),
	    [ Target ! Msg || Target <- Targets ]
    after 15000 ->
	    d("timeout\n"),
	    exit(1)
    end,
    loop(Targets).


