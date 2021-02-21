-module(db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([init/0, loop/1]).

start() ->
	register(db_server, spawn(db, init, [])),
	ok.

stop() ->
	db_server ! {stop, self()},
	receive
		Reply -> Reply
	end.

write(Key, Element) ->
	db_server ! {
		write, 
		self(), 
		{Key, Element}
	},
	receive
		Reply -> Reply
	end.

delete(Key) ->
	db_server ! {delete, self(), Key},
	receive
		Reply -> Reply
	end.

read(Key) -> 
	db_server ! {read, self(), Key},
	receive
		Reply -> Reply
	end.

match(Element) ->
	db_server ! {match, self(), Element},
	receive
		Reply -> Reply
	end.

init() ->
	loop([]).

loop(Db) ->
	receive
		{stop, From} -> From ! stop;
		{delete, From, Key} -> 
			NewDb = deleteImpl(Key, Db),
			From ! ok,
			loop(NewDb);
		{write, From, Data} -> 
			NewDb = writeImpl(Data, Db),
			From ! ok,
			loop(NewDb);
		{match, From, Element} ->
			Res = matchImpl(Element, Db),
			From ! Res,
			loop(Db);
		{read, From, Key} ->
			Res = readImpl(Key, Db),
			From ! Res,
			loop(Db)

	end.

writeImpl(Data, Db) ->
	[Data | Db].

readImpl(_, []) -> {error, instance};
readImpl(Key, [Item|Rest]) ->
	case Item of
		{Key, Element} -> {ok, Element};
		_ -> readImpl(Key, Rest)
	end.

deleteImpl(_, []) -> [];
deleteImpl(Key, [Item|Rest]) -> 
	case Item of
		{Key, _} -> deleteImpl(Key, Rest);
		_ -> [Item|deleteImpl(Key, Rest)]
	end.

matchImpl(_, []) -> [];
matchImpl(Element, [Item | Rest]) ->
	case Item of 
		{Key, Element} -> [Key|matchImpl(Element, Rest)];
		_ -> matchImpl(Element, Rest)
	end.
