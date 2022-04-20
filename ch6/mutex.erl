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
    process_flag(trap_exit, true),
    free().

% 2 states: free->busy, busy->free
free() ->
    receive
        {wait, Pid} ->
            try link(Pid) of
                _ -> Pid ! ok, busy(Pid)
            catch
                error:_Error -> free()
            end;
        stop ->
            terminate()
    end.

busy(Pid) ->
    receive
        {signal, Pid} ->
            catch unlink(Pid),
            Pid ! ok,
            free();
        {'EXIT', Pid, _Reason} ->
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
