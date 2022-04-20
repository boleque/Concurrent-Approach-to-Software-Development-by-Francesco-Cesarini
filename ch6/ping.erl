-module(ping).
-export([start/0, stop/0, send/0, loop/0]).

start() ->
    Pid = spawn_link(ping, loop, []),
    register(ping, Pid),
    {ok, Pid}.

stop() ->
    1/0.

send() ->
    ping ! {self(), ping},
    receive 
        pong -> pong 
    end.

loop() ->
    receive
	{Pid, ping} ->
	    Pid ! pong,
	    loop()
    end.