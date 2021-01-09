-module(ch3).
-export([sum/1, sumRange/2, reverse_create/1, create/1, print/1, filter/2,reverse/1, concatenate/1]).

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
item([El|Rest], Acc) ->
	item(Rest, [El|Acc]).
%---

