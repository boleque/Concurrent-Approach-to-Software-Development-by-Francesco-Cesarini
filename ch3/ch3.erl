-module(ch3).
-export([
	sum/1, 
	sumRange/2, 
	reverse_create/1, 
	create/1, 
	print/1, 
	filter/2,
	reverse/1, 
	concatenate/1, 
	flatten/1,
	quick_sort/1,
	merge_sort/1
]).

% 3-1 Write sum function
%---
sum(0) -> 0;
sum(Num) when is_integer(Num) andalso Num > 0 ->
	Num + sum(Num - 1).
%---
sumRange(Stop, Stop) -> Stop;
sumRange(Start, Stop) when Start =< Stop -> 
	Start + sumRange(Start + 1, Stop);
sumRange(_, _) -> throw({'EXIT', {range_exceeding, [shell, sumRange, 13]}}).

% 3-2 Lists creating
%---
create(Num) -> create(Num, []).
create(Num, Acc) when is_integer(Num) andalso Num > 0 -> create(Num - 1, [Num | Acc]);
create(_, Acc) -> Acc.
%---
reverse_create(0) -> [];
reverse_create(Num) -> 
	[Num | create(Num - 1)].

% 3-3 Side effects
%---
print(Num) when is_integer(Num) -> print(1, Num).
print(Idx, Num) when Idx =< Num -> 
	io:format("Number:~p~n", [Idx]),
	print(Idx + 1, Num);
print(_, _) -> {ok}.

% 3-5 Lists transformation
%---
filter([], _) -> [];
filter([Item|Rest], Num) when Item > Num ->
	filter(Rest, Num);
filter([Item|Rest], Num) ->
	[Item|filter(Rest, Num)].
%---
reverse(List) ->
	reverse(List, []).
reverse([], Acc) -> Acc;
reverse([Head|Rest], Acc) ->
	reverse(Rest, [Head|Acc]).
%---
concatenate(List) ->
	concatenate(List, []).

concatenate([], Acc) -> lists:reverse(Acc);
concatenate([Head|Tail], Acc) ->
	case Head of 
		[] -> concatenate(Tail, Acc);
		_ -> concatenate(Tail, item(Head, Acc))
	end.

item([], Acc) -> Acc;
item([Head|Rest], Acc) ->
	item(Rest, [Head|Acc]).
	
%---
%input: [[[[]]], [1,[2,[3],[]]],[[[4]]],[five, six, [[seven]]]]
%output: [1,2,3,4,five,six,seven]
flatten(List) ->
	flatten(List, []).

flatten([], Acc) -> lists:reverse(Acc);
flatten([Head|Tail], Acc) -> 
	flatten(Tail, get_item(Head, Acc)).

get_item([], Acc) -> Acc;
get_item([Head|Tail], Acc) when is_list(Head) ->
	get_item(Tail, get_item(Head, Acc));

get_item([Head|Tail], Acc) ->
	get_item(Tail, [Head|Acc]).

% 3-6 Sorting
%---
quick_sort([]) -> [];
quick_sort([Pivot|Tail]) ->
	quick_sort([Val || Val <- Tail, Val =< Pivot]) ++ [Pivot] ++ quick_sort([Val || Val <- Tail, Val > Pivot]).
%---
merge_sort([Val]) -> [Val];
merge_sort(List) -> 
	{LHalf, RHalf} = lists:split(length(List) div 2, List),
	merge(merge_sort(LHalf), merge_sort(RHalf)).

merge(LHalf, RHalf) -> merge(LHalf, RHalf, []).
merge([], [H|T], Acc) -> merge([], T, [H|Acc]);
merge([H|T], [], Acc) -> merge(T, [], [H|Acc]);
merge([Hl|Tl], [Hr|Tr], Acc) ->
	if 
		Hl =< Hr -> merge(Tl, [Hr|Tr], [Hl|Acc]);
		true     -> merge([Hl|Tl], Tr, [Hr|Acc])
	end;
merge(_, _, Acc) -> lists:reverse(Acc).

