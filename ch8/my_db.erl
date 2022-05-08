-module(my_db).
-export([start/0, stop/0, upgrade/1]).
-export([write/2, read/1, delete/1]).
-export([code_upgrade/0, init/0, loop/1]).
-vsn(1.0).

start() ->
  register(my_db, spawn(my_db, init, [])).

stop()->
  my_db ! stop.

upgrade(Data) ->
  my_db ! {upgrade, Data}.

write(Key, Data) ->
  my_db ! {write, Key, Data}.

read(Key) ->
  my_db ! {read, self(), Key},
  receive Reply -> Reply end.

delete(Key) ->
  my_db ! {delete, Key}.

code_upgrade() ->
	my_db ! {code_upgrade, self()}.

init() ->
  loop(db:new()).

loop(Db) ->
  receive
	{code_upgrade, _Pid} ->
		loop(db:code_upgrade(Db));
    {write, Key, Data} ->
       loop(db:write(Key, Data, Db));
    {read, Pid, Key} ->
       Pid ! db:read(Key, Db),
       loop(Db);
    {delete, Key} ->
       loop(db:delete(Key, Db));
    {upgrade, Data} ->
      NewDb = db:convert(Data, Db),
      db_server:loop(NewDb);
    stop ->
      db:destroy(Db)
  end. 


