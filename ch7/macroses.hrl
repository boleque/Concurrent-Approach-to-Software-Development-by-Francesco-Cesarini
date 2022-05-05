% 7-6
-ifdef(show).
    -define(SHOW_EVAL(EXPR), io:format("~p~n", [??EXPR])).
-else.
    -define(SHOW_EVAL(EXPR), EXPR).
-endif.

% 7-7
-define(COUNT_FUN_CALL(FUN), io:format("~p:~p called ~n", [?MODULE, ??FUN])).

% 7-8
-define(MONDAY, {day, 1}).
-define(TUESDAY, {day, 2}).
-define(WEDNESDAY, {day, 3}).
-define(THURSDAY, {day, 4}).
-define(FRIDAY, {day, 5}).
-define(SATURDAY, {day, 6}).
-define(SUNDAY, {day, 7}).
