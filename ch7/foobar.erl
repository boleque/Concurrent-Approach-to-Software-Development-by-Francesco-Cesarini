-module(foobar).
-export([foobar/1]).

-record(person, {name,age=0,phone,address}).

foobar(#person{name="Joe"} = P) when is_record(P, person) ->
    io:format("Record for Joe~n");

foobar(_P) ->
    io:format("Not a record for Joe~n").