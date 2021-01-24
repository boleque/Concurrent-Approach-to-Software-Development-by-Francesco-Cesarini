-module(echo).
-export([start/0, stop/0, loop/0, print/1]).

start() ->
   register(?MODULE, spawn(?MODULE, loop, [])),
   ok.

stop() ->
    ?MODULE ! stop,
    ok.

print(Term) ->
    ?MODULE ! {print, Term},
    ok.

loop() ->
   receive
     {print, Msg} ->
        io:format("~p~n", [Msg]),
        loop();
     stop ->
       ok
   end.