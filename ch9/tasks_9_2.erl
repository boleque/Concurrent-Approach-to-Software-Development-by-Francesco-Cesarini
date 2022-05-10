-module(tasks_9_2).
-export([div_by_3/2]).
-export([filter_integers/1]).
-export([intersection/2]).
-export([symmetric_difference/2]).


%%
div_by_3(From, To) ->
    [X || X <- lists:seq(From, To), X rem 3 == 0].

%%
filter_integers(Seq) ->
    [X || X <- Seq, is_integer(X)].

%%
intersection(Seq1, Seq2) ->
    [X || X <- Seq1, lists:member(X, Seq2)].

%%
symmetric_difference(Seq1, Seq2) ->
    [X || X <- Seq1, not(lists:member(X, Seq2))] ++ [Y || Y <- Seq2, not(lists:member(Y, Seq1))].
