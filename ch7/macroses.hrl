-ifdef(show).
    -define(SHOW_EVAL(EXPR), io:format("~p~n", [??EXPR])).
-else.
    -define(SHOW_EVAL(EXPR), EXPR).
-endif.

-define(COUNT_FUN_CALL(FUN), io:format("~p:~p called ~n", [?MODULE, ??FUN])).