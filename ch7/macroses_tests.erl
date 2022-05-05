-module(macroses_tests).
-export([test_show_eval/0]).
-include("macroses.hrl").


test_show_eval() ->
    ?SHOW_EVAL(length([1,2,3])).