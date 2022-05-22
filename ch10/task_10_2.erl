-module(index).
-export([processWords/2]).


processWords(Words,N) ->
  case Words of
    [] -> ok;
    [Word|Rest] ->
      WordConverted = binary_to_list(Word),
      if
        length(WordConverted) > 3 ->
            Normalise = string:to_lower(WordConverted),
            case ets:lookup(indexTable, WordConverted) of
                [{_, Lines}] ->
                    ets:insert(indexTable, {WordConverted, [N|Lines]});
                [] -> 
                    ets:insert(indexTable, {WordConverted, [N]})
            end,
            ets:insert(indexTable,{{Normalise , N}});
        true -> ok
      end,
      processWords(Rest,N)
  end.