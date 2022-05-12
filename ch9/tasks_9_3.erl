-module(tasks_9_3).
-export([zip/2]).
-export([zipWith/3]).


%%
zip([], _) -> [];

zip(_, []) -> [];

zip([X|Rx], [Y|Ry]) ->
    [{X, Y} | zip(Rx, Ry)].

%%
zipWith(Fun, Seq1, Seq2) ->
    [Fun(X, Y) || {X, Y} <- zip(Seq1, Seq2)].