-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, code_upgrade/1]).
-export([etsfy/2]).
-vsn(1.1).


new() -> gb_trees:empty().

write(Key, Data, Db) -> 
  gb_trees:insert(Key, Data, Db).

read(Key, Db) ->
  case gb_trees:lookup(Key, Db) of
    none         -> {error, instance};
    {value, Data} -> {ok, Data}
  end.

destroy(_Db) -> ok.

delete(Key, Db) ->
  gb_trees:delete(Key, Db).

code_upgrade([]) ->
  new();
code_upgrade(Db) ->
  etsfy(Db, new()).

etsfy([{Key, Data}|Rest], GbTree) ->
  NewGbTree = gb_trees:insert(Key, Data, GbTree),
  etsfy(Rest, NewGbTree);
etsfy([], GbTree) -> GbTree.
