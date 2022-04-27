-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
  register(Name, spawn_link(my_supervisor, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
  process_flag(trap_exit, true),
  loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{M, F, A, ProcType, FirstFallTime, FallsNum} | ChildSpecList]) ->
  case (catch apply(M,F,A)) of
    {ok, Pid} ->
      [{Pid, {M,F,A,ProcType,FirstFallTime,FallsNum}}|start_children(ChildSpecList)];
    _ ->
      start_children(ChildSpecList)
  end.

%% The loop of the supervisor waits in a receive clause for EXIT and stop messages. 
%% If a child terminates, the supervisor receives the EXIT signal and restarts the terminated 
%% child, replacing its entry in the list of children stored in the ChildList variable:

restart_child(Pid, ChildList, Reason) ->
  {value, {Pid, {M,F,A,ProcType,FirstFallTime,FallsNum}}} = lists:keysearch(Pid, 1, ChildList),
  IsTypeRestartable = case {ProcType, Reason} of
                    {permanent, _} -> true;
                    {transient, normal} -> false;
                    {transient, _Other} -> true
                  end,
  TimeNow = calendar:time_to_seconds(time()), % sec
  IsEnoughAttempts = case {FirstFallTime, FallsNum} of
                    {0, _} -> 
                      io:format("First fall, module ~p~n", [M]),
                      NewFirstFallTime = TimeNow, 
                      NewFallsNum = FallsNum - 1,
                      true;
                    {_, 0} ->
                      io:format("Last fall, module ~p~n", [M]),
                      NewFallsNum = FallsNum,
                      NewFirstFallTime = FirstFallTime,
                      false;
                    _ ->
                      TimeDiff = TimeNow - FirstFallTime,
                      io:format("Fall number: ~p, module ~p, timediff ~p~n", [FallsNum, M, TimeDiff]),
                      if
                        TimeDiff < 60 ->
                          NewFirstFallTime = FirstFallTime,
                          NewFallsNum = FallsNum - 1,
                          true;
                        true ->
                          NewFirstFallTime = TimeNow,
                          NewFallsNum = 5,
                          true
                      end
                  end,
  OldPidRemovedChildList = lists:keydelete(Pid,1,ChildList),
  if
    IsTypeRestartable and IsEnoughAttempts ->
      {ok, NewPid} = apply(M,F,A),
      [{NewPid, {M,F,A,ProcType,NewFirstFallTime,NewFallsNum}} | OldPidRemovedChildList];
    true -> OldPidRemovedChildList
  end.

loop([]) ->
  io:format("There aren`t processes to restart~n", []);

loop(ChildList) ->
  receive
    {'EXIT', Pid, Reason} ->
      NewChildList = restart_child(Pid, ChildList, Reason),
      loop(NewChildList);
    {stop, From}  ->
      From ! {reply, terminate(ChildList)}
  end.

%% We stop the supervisor by calling the synchronous client function stop/0. Upon receiving the 
%% stop message, the supervisor runs through the ChildList, terminating the children one by one.
%% Having terminated all the children, the atom ok is returned to the process that initiated 
%% the stop call:

stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end.

terminate([{Pid, _} | ChildList]) ->
  exit(Pid, kill),
  terminate(ChildList);
terminate(_ChildList) -> ok.
