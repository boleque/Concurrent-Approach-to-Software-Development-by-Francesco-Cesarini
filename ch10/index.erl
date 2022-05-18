-module(index).

-export([index/1,processFile/1,prettyEntry/1]).
-export([accumulate/1, accumulate/2]).
-export([prettyList/1]).
-export([pad/2]).
-export([test/0]).


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
  case re:split(Line,?Punctuation) of
    Words ->
      processWords(Words,N);
    _ ->
    []
  end.

processWords(Words,N) ->
  case Words of
    [] -> ok;
    [Word|Rest] ->
      %io:format("Here Iam LEN:~p~n", [length(binary_to_list(Word))]),
      WordConverted = binary_to_list(Word),
      if
        length(WordConverted) > 3 ->
          Normalise = string:to_lower(WordConverted),
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

prettyEntry({Word, Lines}) ->
  io:put_chars(pad(20, Word)),
  prettyList(accumulate(Lines)),
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

%%
pad(N,Word) ->
  Len = length(Word),
  if
	  Len >= N -> Word;
	  true -> Word ++ add_spaces(N-Len)
  end.

add_spaces(0) ->
  [];

add_spaces(N) ->
  " " ++ add_spaces(N-1).

test() ->
    index("Text.txt").

%lorem               1-2,6,8.
%make                3.
%more                7.
%only                4.
%pagemaker           7.
%passages            6.
%popularised         5.
%printer             3.
%printing            1.
%publishing          7.
%recently            7.
%release             6.
%remaining           5.
%scrambled           3.
%sheets              6.
%simply              1.
%since               2.
%software            7.
%specimen            4.
%standard            2.
%survived            4.
%text                1-2.
%took                3.
%type                3.
%typesetting         1,5.
%unchanged           5.
%unknown             3.
%versions            8.
%when                3.
%with                6-7.

    
