-module(db).
-export([new/0,write/3,read/2,delete/2,match/2]).
-vsn(1.0).


new() ->
	NewDb = [],
    NewDb.

write(Key, Data, Db) ->
	[{Key, Data} | Db].

read(_, []) -> {error, instance};
read(Key, [Item|Rest]) ->
	case Item of
		{Key, Element} -> {ok, Element};
		_ -> read(Key, Rest)
	end.

delete(_, []) -> [];
delete(Key, Db) ->
	delete_acc(Key, Db, []).

delete_acc(_Key, [], Acc) -> 
	Acc;
delete_acc(Key, [Item | Rest], Acc) ->
	case Item of
		{Key, _} -> delete_acc(Key, Rest, Acc);
		_ -> delete_acc(Key, Rest, [Item | Acc])
	end.


match(Element, []) -> {error, Element};
match(Element, Db) ->
	Res = match_acc(Element, Db, []),
	case Res of
		[] -> {error, Element};
		_ -> Res
	end.

match_acc(_Element, [], Acc) ->
	Acc;

match_acc(Element, [Item | Rest], Acc) ->
	case Item of
		{Key, Element} -> match_acc(Element, Rest, [Key | Acc]);
		_ -> match_acc(Element, Rest, Acc)
	end.