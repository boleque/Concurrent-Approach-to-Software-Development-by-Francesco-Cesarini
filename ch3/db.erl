-module(db).
-export([new/0]).
-export([write/3]).
-export([read/2]).
-export([delete/2]).
-export([destroy/1]).
-export([match/2]).

new() -> [].

write(Key, Item, Db) ->
	[{Key, Item} | Db].

read(Key, []) -> {key_error, Key};
read(Key, [Item|Rest]) ->
	case Item of
		{Key, Value} -> {ok, Value};
		_ -> read(Key, Rest)
	end.

delete(_, []) -> [];
delete(Key, [Item|Rest]) -> 
	case Item of
		{Key, Value} -> delete(Key, Rest);
		_ -> [Item|delete(Key, Rest)]
	end.

destroy(Db) ->
	{ok}.

match(_, []) -> [];
match(Element, [Item | Rest]) ->
	case Item of 
		{Key, Element} -> [Key|match(Element, Rest)];
		_ -> match(Element, Rest)
	end.
	