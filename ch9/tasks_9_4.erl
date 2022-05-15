-module(tasks_9_4).
-export([map/2]).
-export([foldl/3]).
-export([foldr/3]).


%%
map(Fun, Seq) ->
    map_acc(Fun, Seq, []).


map_acc(_Fun, [], Acc) ->
    Acc;

map_acc(Fun, [Hd|Tail], Acc) ->
    map_acc(Fun, Tail, [Hd|Acc]).

%% tail recursive, traverse from left to right
foldl(_Fun, Acc, []) ->
    Acc;

foldl(Fun, Acc, [Hd|Tail]) ->
    foldl(Fun, Fun(Hd, Acc), Tail).

%% not tail recursive, traverse from right to left
foldr(_Fun, Acc, []) ->
    Acc;

foldr(Fun, Acc, [Hd|Tail]) ->
    Fun(Hd, foldr(Fun, Acc, Tail)).