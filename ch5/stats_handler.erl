-module(stats_handler).
-export([init/1, terminate/1, handle_event/2]).


init(Stats) ->
    Stats.

terminate(Stats) -> {stats, Stats}.

handle_event({Type, Id, Description}, Stats) ->
    case lists:keyfind(Key={Type, Description}, 1, Stats) of
        {Key, Count} -> 
            lists:keyreplace(Key, 1, Stats, {Key, Count + 1});
        false -> 
            [{Key, 1}|Stats]
    end;

handle_event(_, Stats) ->
  Stats.