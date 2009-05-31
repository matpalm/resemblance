-module(broadcast_router).
-compile(export_all).
-include("debug.hrl").

start(Targets) ->
    spawn(?MODULE,loop,[Targets]).

loop(Targets) ->
%    d(">loop across ~w\n",[Targets]),
    receive
	{ ack,Pid } ->
	    Pid ! { ack,self() },
	    loop(Targets);

	Msg ->
%	    d("forwarding ~w\n",[Msg]),
	    [ Target ! Msg || Target <- Targets ]

    end,
    loop(Targets).


