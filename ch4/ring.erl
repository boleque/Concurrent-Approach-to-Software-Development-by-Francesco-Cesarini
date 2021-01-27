-module(ring).
-export([start/3]).

start(M, N, Msg) ->
    {ok, Pid} = start_proc(N-1, self()),
    ok = sendMessage(M, Pid, Msg),
    Pid ! quit,
    receive
        quit -> {ok, quit}
    end.

start_proc(0, Pid) -> {ok, Pid};
start_proc(N, Pid) ->
    start_proc(N-1, spawn(fun() -> loop(Pid) end)).

sendMessage(0, _, _) -> ok;
sendMessage(M, Pid, Msg) ->
    Pid ! {message, Msg}, 
    receive 
        {message, GotMsg} -> sendMessage(M-1, Pid, GotMsg)
    end.

loop(Pid) ->
    receive
        {message, _} = Msg -> Pid ! Msg, loop(Pid);
        quit -> Pid ! quit
    end.