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
	merge_sort/1,
	eval/1,
	make_indexing/1,
	format/2
]).

% 3-1 Write sum function
%---
sum(Num) -> sum_acc(Num, 0).

sum_acc(0, Acc) -> Acc;
sum_acc(Num, Acc) -> sum_acc(Num-1, Acc+Num).

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
	case is_list(Head) of
		true -> flatten(Head, Acc);
		false -> flatten(Tail, [Head | Acc])
	end.

%%%%%%%%%%%



% 3-6 Sorting
%---
quick_sort([]) -> [];
quick_sort([Head|Tail]) ->
	% Head is the pivot
	Left_half = [Val || Val <- Tail, Val =< Head],
	Right_half = [Val || Val <- Tail, Val > Head],
	lists:merge3(quick_sort(Left_half), [Head], quick_sort(Right_half)).

%---
merge_sort(List) when length(List) < 2 -> List;
merge_sort(List) -> 
	{Left_half, Right_half} = lists:split(length(List) div 2, List),
	merge(
		merge_sort(Left_half), 
		merge_sort(Right_half)
	).

merge(Left_half, Right_half) -> merge_acc(Left_half, Right_half, []).

merge_acc([], [], Acc) -> lists:reverse(Acc);
merge_acc([], [H_R|T_R], Acc) -> merge_acc([], T_R, [H_R|Acc]);
merge_acc([H_L|T_L], [], Acc) -> merge_acc(T_L, [], [H_L|Acc]);

merge_acc([H_L|T_L], [H_R|T_R], Acc) when H_L < H_R ->
	merge_acc(T_L, [H_R|T_R], [H_L|Acc]);

merge_acc([H_L|T_L], [H_R|T_R], Acc) ->
	merge_acc([H_L|T_L], T_R, [H_R|Acc]).



% 3-8 Calculator
% todo: ~(), if ... then ..

eval(Exps) ->
	compilator(parse(Exps)).

compilator(ParsedExps) ->
	Ops = compilator(ParsedExps, [], []),
	transformer(lists:reverse(Ops)).

compilator([], Ops, _) -> Ops;
compilator([{num, Num}|T], Ops, Tokens) -> 
	compilator(T, [{num, Num} | Ops], Tokens);

compilator([{br, ')'}|T], Ops, []) ->
	compilator(T, Ops, []);

compilator([{br, ')'}|T], Ops, Tokens) ->
	{Op, NewStack2} = last_op(Tokens),
	compilator(T, [Op|Ops], NewStack2);

compilator([H|T], Ops, Tokens) ->
	compilator(T, Ops, [H|Tokens]).

transformer(Ops) -> transformer(Ops, []).
transformer([], Stack) -> Stack;

transformer([{num, Num}|T], Stack) ->
	transformer(T, [Num|Stack]);

transformer([{op, Op}|T], Stack) ->
	[Num1, Num2|R] = Stack,
	transformer(T, [simulator(Op, Num1, Num2)|R]).

simulator(Op, Num1, Num2) -> 
	case Op of
		'+' -> Num1 + Num2;
		'-' -> Num1 - Num2;
		'/' -> Num1 / Num2;
		'*' -> Num1 * Num2;
		Any -> logger:warning("Got unknown operator ~p", [Any])
	end.

parse(Str) -> parse(Str, []).

parse([$(|T], Acc) -> parse(T, [{br, '('}|Acc]);
parse([$)|T], Acc) -> parse(T, [{br, ')'}|Acc]);
parse([$-|T], Acc) -> parse(T, [{op, '-'}|Acc]); 
parse([$+|T], Acc) -> parse(T, [{op, '+'}|Acc]);
parse([$*|T], Acc) -> parse(T, [{op, '*'}|Acc]);
parse([$/|T], Acc) -> parse(T, [{op, '/'}|Acc]);
parse([], Acc) -> lists:reverse(Acc);

parse([H|T], Acc) when H =< $9, H >= $0 ->
	{Num, Rest} = parse_num([H|T], 0),
	parse(Rest, [{num, Num}|Acc]).

parse_num([H|T], Res) when H =< $9, H >= $0 ->
	parse_num(T, Res*10 + H - $0);

parse_num([$.|T], Res) ->
	{Fract, Rest} = parse_fract(T, 0, 1),
	{Res + Fract, Rest};

parse_num([H|_]=L, Res) when H > $9; H < $0 ->
	{Res, L}.
	
parse_fract([H|T], Res, Fract) when H =< $9, H >= $0 ->
	parse_fract(T, Res*10 + H - $0, Fract*0.1);

parse_fract([H|_]=L, Res, Fract) when H > $9; H < $0 ->
	{Res*Fract, L}.

last_op([Op|T]) ->
	last_op(Op, T).
last_op(Op, [{br, '('}|T]) ->
	{Op, T};
last_op(Op, L) ->
	{Op, L}.

% 3-9 Indexing
make_indexing(FileName) -> 
	FileData = read_file(FileName),
	make_indexing(FileData, 1, maps:new()).

make_indexing([], _, Acc) ->
	maps:map(fun(_, V) -> zip(V, []) end, Acc);

make_indexing([Words|T], Row, Acc) ->

	Get_entries = fun(Word, Res) ->
		Lines = maps:get(Word, Res, []),
		maps:put(Word, Lines ++ [Row], Res)
	end,

	NewAcc = lists:foldl(
		Get_entries,
		Acc,
		Words
	),
	make_indexing(T, Row + 1, NewAcc).

read_file(FileName) ->
	{ok, IODev} = file:open(FileName, read),
	Data = read_file({io, IODev}, []),
	file:close(IODev),
	lists:reverse(Data).

read_file({io, IODev}, Acc) ->
	Line = io:get_line(IODev, ''),
	case Line of
		eof -> Acc;
		_ ->
			WordsList = string:split(string:strip(Line, right, $\n), " ", all),
			read_file({io, IODev}, [WordsList|Acc])
	end.

zip(Src, Res) ->
	case get_range(Src, 0, 0) of
		{{Start, 0}, []} -> [Start|Res];
		{{Start, 0}, Rest} -> zip(Rest, [Start|Res]);
		{{Start, End}, []} -> [{Start, End}|Res];
		{{Start, End}, Rest} -> zip(Rest, [{Start, End}|Res])
	end.
 
get_range([], Start, End) -> {{Start, End}, []};
get_range([H|T], 0, 0) -> get_range(T, H, 0);
get_range([H|T], Start, 0) when Start =:= H-1 -> get_range(T, Start, H);
get_range([H|T], H, 0) -> get_range(T, H, 0);
get_range([H|T], Start, End) when End =:= H-1 -> get_range(T, Start, H);
get_range(L, Start, End) -> {{Start, End}, L}.

% 3-10
format(FileName, Limit) ->
	FileData = read_file(FileName),
	format(FileData, Limit, []).

format([[]], _, Acc) -> lists:reverse(Acc);
format([Line|T]=L, Limit, Acc) when length(T) =:= 0 ->
	{Formed, Rest} = form_line(Line, Limit, 0, []),
	format([Rest|[]], Limit, [Formed|Acc]);

format([Line|T], Limit, Acc) ->
	{Formed, Rest} = form_line(Line, Limit, 0, []),
	[NewLine|NT] = T,
	format([Rest++NewLine|NT], Limit, [Formed|Acc]).

form_line([], _, _, Res) -> {Res, []};
form_line([Word|T], Limit, Counter, Res) when Counter + length(Word) =< Limit ->
	form_line(T, Limit, Counter+length(Word), [Word|Res]);

form_line([Word|_]=L, Limit, Counter, Res) when Counter + length(Word) > Limit ->
	{Res, L}.