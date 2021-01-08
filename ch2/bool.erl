-module(bool).
-export([b_not/1, b_and/2, b_or/2, b_nand/2]).

b_not(Arg) ->
	case Arg of
		true -> false;
		false -> true
	end.

b_and(Arg1, Arg2) ->
	case {Arg1, Arg2} of
		{true, true} -> true;
		_ -> false
	end.

b_or(Arg1, Arg2) ->
	case {Arg1, Arg2} of
		{true, _} -> true;
		{_, true} -> true;
		_ -> false
	end.
	
b_nand(Arg1, Arg2) ->
	b_not(b_and(Arg1, Arg2)).