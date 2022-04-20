-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
    register(mutex, spawn(?MODULE, init, [])).

stop() ->
    mutex ! stop.

wait() ->
    mutex ! {wait, self()},
    receive ok -> ok end.

signal() ->
    mutex ! {signal, self()}, ok.

init() ->
    free().

% 2 states: free->busy, busy->free
free() ->
    receive
        {wait, Pid} ->
            % todo link with Pid (try/catch in case Pid is exists)
            Pid ! ok,
            busy(Pid);
        stop ->
            terminate()
    end.

busy(Pid) ->
    receive
        {signal, Pid} ->
            % todo unlink with Pid (try/catch in case Pid is not exists)
            free()
    end.

terminate() ->
    receive
        {wait, Pid} ->
            exit(Pid, kill),
            terminate()
        after
            0 -> ok
    end.
