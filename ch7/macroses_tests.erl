-module(macroses_tests).
-export([test_show_eval/0, test_calc_fun_call/0]).
-include("macroses.hrl").


test_show_eval() ->
    ?SHOW_EVAL(length([1,2,3])).


test_calc_fun_call() ->
    ?COUNT_FUN_CALL(test_calc_fun_call).