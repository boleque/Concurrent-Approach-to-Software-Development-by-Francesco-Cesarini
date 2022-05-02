-file("records.erl", 1).

-module(records).

-export([birthday/1,joe/0,showPerson/1]).

-record(person,{name, age = 0, phone, address}).

birthday({person, _, Age, _, _} = P) ->
    begin
        rec0 = Age + 1,
        rec1 = P,
        case rec1 of
            {person, _, _, _, _} ->
                setelement(3, rec1, rec0);
            _ ->
                error({badrecord, person})
        end
    end.

joe() ->
    {person, "Joe", 21, "999-999", "Tallinn"}.

showPerson({person, Name, Age, Phone, Address}) ->
    io:format("name: ~p  age: ~p  phone: ~p address: ~p ~n",
              [Name, Age, Phone, Address]).



