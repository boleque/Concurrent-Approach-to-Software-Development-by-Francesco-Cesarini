- module(tasks_9_1).
- export([print_range/2]).
- export([get_smaller/2]).
- export([print_even/2]).


%%%
print_range(From, To) ->
    Predicat = fun(X) -> io:format("Element ~p~n", [X]) end,
    lists:map(Predicat, lists:seq(From, To)).

%%% 
get_smaller(Num, Seq) ->
    Predicat = fun(X) -> X < Num end,
    lists:filter(Predicat, Seq).

%%%
print_even(From, To) ->
    ListOfEvens = lists:filter(
        fun(X) -> X rem 2 == 0 end,
        lists:seq(From, To)
    ),
    lists:map(
        fun(X) -> io:format("Element ~p~n", [X]) end, 
        ListOfEvens
    ).

