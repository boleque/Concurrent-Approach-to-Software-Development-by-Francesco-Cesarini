-module(calc).
-export([eval/1]).

eval(Exps) ->
	compilator(parse(Exps)).

compilator(ParsedExps) ->
	io:format("Parsed ~p~n", [ParsedExps]),
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

last_op(Op, [H|List]) ->
	io:format("last_op ~p~n", [H]),
	{Op, List}.