-module(mnode).
-compile(export_all).

start() ->
    start(["192.168.0.2"]).
    
start(Servers) ->
    erl_boot_server:start(Servers),
    wait_until_servers_up(length(Servers)+1). %+1 for self

restart(Servers) ->
    [ rpc:call(Node,init,restart,[]) || Node <- nodes() ],
    wait_until_servers_up(length(Servers)+1). %+1 for self

wait_until_servers_up(NumRequired) ->
    Servers = net_adm:world(),
    case length(Servers) == NumRequired of
	false ->
	    timer:sleep(1000),
	    io:format("waiting for all ~p servers to be up, currently ~p\n",[NumRequired,length(Servers)]),
	    wait_until_servers_up(NumRequired);
	true -> 
	    done
    end.
		      

next_node() ->
    case node() of
	'nonode@nohost' ->
	    node(); % running as single node, spawn everything locally
	_ ->
	    case get(nodes) of
		undefined -> next_node([]);
		Nodes -> next_node(Nodes)
	    end
    end.

next_node([]) ->
    next_node(net_adm:world());
next_node([H|T]) ->
    put(nodes, T),
    H.
	     
