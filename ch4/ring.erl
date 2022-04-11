-module(ring).
-export([start/3]).
 

loop(ChildPid) ->
    receive
        quit ->
            ChildPid ! quit;
        {message, 0, _Msg} -> 
            io:format("Received the last message, finish message handling...~n"),
            ChildPid ! quit;
        {message, M, Msg} ->
            io:format("Received the message M=~p~n", [M]),
            ChildPid ! {message, M-1, Msg}, 
            loop(ChildPid)
    end.


start(M, N, Msg) ->
    io:format("Start the parent process~n"),
    ParentPid = self(),
    ChildPid = spawn(fun() -> start_process(N-1, ParentPid) end),
    receive
        {ring_complete} -> ChildPid ! {message, M-1, Msg}, loop(ChildPid)
    end,
    io:format("Stop the parent process~n").


start_process(0, ParentPid) ->
    io:format("Start last child process~n"),
    ParentPid ! {ring_complete},
    loop(ParentPid),
    io:format("Stop the last child process~n");


start_process(N, ParentPid) ->
    io:format("Start child process=~p~n", [N]),
    ChildPid = spawn(fun() -> start_process(N-1, ParentPid) end),
    loop(ChildPid),
    io:format("Stop the process with N=~p~n", [N]).





