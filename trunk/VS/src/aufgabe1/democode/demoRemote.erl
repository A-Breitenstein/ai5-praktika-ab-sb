-module(demoRemote).
-compile(export_all).

start(Name) ->
	ServerPid = spawn(fun() -> loop([]) end),
    register(Name,ServerPid),
	ServerPid.

rpc(Pid, Query) ->
     Pid ! {self(), Query},
     receive {PID,Reply} -> io:format("Received Reply from ~p:~p\n", [PID,Reply]),
                            io:format("Done\n");
	          Any -> io:format("Received Something:~p\n", [Any])
	 end.

loop(X) ->
     receive {PID,Query} -> io:format("Received Query:~p ! ~p\n", [PID,Query]),
	                        PID ! {self() , Query},
							loop(X);
	         Any -> io:format("Received Something:~p\n", [Any]),
                    loop(X)
     end.