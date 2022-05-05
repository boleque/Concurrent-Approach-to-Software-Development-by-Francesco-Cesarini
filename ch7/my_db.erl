-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([init/0, loop/1]).

-ifdef(debug).
	-define(DBG(Str, Arg), io:format(Str, Arg)).
-else.
	-define(DBG(Str, Arg), ok).
-endif.

-record(data, {key, data}).


%%%
start() ->
	register(db_server, spawn(my_db, init, [])),
	ok.

stop() ->
	call(stop).

write(Key, Element) ->
	Data = #data{key=Key, data=Element},
	call({write, Data}).

delete(Key) ->
	call({delete, Key}).

read(Key) ->
	call({read, Key}).

match(Element) ->
	call({match, Element}).

%%%
call(Message) ->
	db_server ! {self(), Message},
	receive
		{reply, Reply} -> ?DBG("~p:call(~p) called~n", [?MODULE, Reply])
	end.

init() ->
	loop([]).

loop(Db) ->
	receive
		{Pid, stop} -> 
			Pid ! {reply, ok};
		{Pid, {delete, Key}} -> 
			NewDb = delete_impl(Key, Db),
			Pid ! {reply, ok},
			loop(NewDb);
		{Pid, {write, Data}} ->
			NewDb = write_impl(Data, Db),
			Pid ! {reply, ok},
			loop(NewDb);
		{Pid, {match, Element}} ->
			Res = match_impl(Element, Db),
			Pid ! {reply, Res},
			loop(Db);
		{Pid, {read, Key}} ->
			Res = read_impl(Key, Db),
			Pid ! {reply, Res},
			loop(Db)
	end.

write_impl(Data, Db) ->
	[Data | Db].

read_impl(_, []) -> {error, instance};
read_impl(Key, [Item|Rest]) ->
	case Item of
		#data{key=Key, data=Element} -> {ok, Element};
		_ -> read_impl(Key, Rest)
	end.

delete_impl(_, []) -> [];
delete_impl(Key, Db) ->
	delete_acc(Key, Db, []).

delete_acc(_Key, [], Acc) -> 
	Acc;
delete_acc(Key, [Item | Rest], Acc) ->
	case Item of
		#data{key=Key, data=_Element} -> delete_acc(Key, Rest, Acc);
		_ -> delete_acc(Key, Rest, [Item | Acc])
	end.


match_impl(Element, []) -> {error, Element};
match_impl(Element, Db) ->
	Res = match_acc(Element, Db, []),
	case Res of
		[] -> {error, Element};
		_ -> Res
	end.

match_acc(_Element, [], Acc) ->
	Acc;

match_acc(Element, [Item | Rest], Acc) ->
	case Item of
		#data{key=Key, data=Element} -> match_acc(Element, Rest, [Key | Acc]);
		_ -> match_acc(Element, Rest, Acc)
	end.