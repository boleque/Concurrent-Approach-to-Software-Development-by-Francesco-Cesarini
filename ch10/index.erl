-module(index).

-export([index/1,processFile/1,prettyEntry/1]).
-export([accumulate/1, accumulate/2]).
-export([prettyList/1]).


index(File) ->
  ets:new(indexTable, [ordered_set, named_table]),
  processFile(File),
  prettyIndex().

processFile(File) ->
  {ok,IoDevice} = file:open(File,[read]),
  processLines(IoDevice,1).

processLines(IoDevice,N) ->
  case io:get_line(IoDevice,"") of
    eof ->
      ok;
    Line ->
      processLine(Line,N),
      processLines(IoDevice,N+1)
  end.
					 
-define(Punctuation,"(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").

processLine(Line,N) ->
  case regexp:split(Line,?Punctuation) of
    {ok,Words} ->
      processWords(Words,N) ;
    _ -> []
  end.

processWords(Words,N) ->
  case Words of
    [] -> ok;
    [Word|Rest] ->
      if
        length(Word) > 3 ->
          Normalise = string:to_lower(Word),
          ets:insert(indexTable,{{Normalise , N}});
        true -> ok
      end,
      processWords(Rest,N)
  end.

prettyIndex() ->
  case ets:first(indexTable) of
    '$end_of_table' ->
      ok;
    First  ->
      case First of
        {Word, N} ->
          IndexEntry = {Word, [N]}
      end,
      prettyIndexNext(First,IndexEntry)
  end.


prettyIndexNext(Entry,{Word, Lines}=IndexEntry) ->
  Next = ets:next(indexTable,Entry),
  case Next of
    '$end_of_table' ->
      prettyEntry(IndexEntry);
    {NextWord, M}  ->
      if
        NextWord == Word ->
          prettyIndexNext(Next,{Word, [M|Lines]});
        true ->
          prettyEntry(IndexEntry),
          prettyIndexNext(Next,{NextWord, [M]})
      end
  end.

prettyEntry(IndexEntry) ->
  % exercise
  ok.

%%
accumulate(Seq) ->
  accumulate(Seq, []).

accumulate([], Acc) ->
  Acc;

accumulate([N|NRest], []) ->
  accumulate(NRest, [{N}]);

accumulate([NextEl|NRest], [{StartEl}|PRest]=Acc) ->
  if
    StartEl == NextEl ->
      accumulate(NRest, [{StartEl}|PRest]);
    StartEl == NextEl + 1 ->
      accumulate(NRest, [{NextEl,StartEl}|PRest]);
    true ->
      accumulate(NRest, [{NextEl}|Acc])
  end;

accumulate([NextEl|NRest], [{EndEl,StartEl}|PRest]=Acc) ->
  if
    EndEl == NextEl ->
      accumulate(NRest, [{EndEl,StartEl}|PRest]);
    EndEl == NextEl + 1 ->
      accumulate(NRest, [{NextEl,StartEl}|PRest]);
    true ->
      accumulate(NRest, [{NextEl}|Acc])
  end.

%%
prettyList([]) ->
  ok;

prettyList([{StartEl}]) ->
  io:format("~p.~n",[StartEl]);

prettyList([{StartEl, EndEl}]) ->
  io:format("~p-~p.~n",[StartEl,EndEl]);

prettyList([{StartEl, EndEl}|Rest]) ->
  io:format("~p-~p,",[StartEl,EndEl]),
  prettyList(Rest);

prettyList([{StartEl}|Rest]) ->
  io:format("~p,",[StartEl]),
  prettyList(Rest).





    
