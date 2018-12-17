-module(main).
-export([solution_1/0, solution_2/0]).

solution_1() ->
  Lines = readlines('input.txt'),
  Lines1 = lists:map(fun string:trim/1, Lines),
  Maps = lists:map(fun frequency_map/1, Lines1),
  Count_3 = count_3_frequency_ids(Maps),
  Count_2 = count_2_frequency_ids(Maps),
  Count_3 * Count_2.

readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case file:read_line(Device) of
      eof  -> [];
      {ok, Line} -> [Line] ++ get_all_lines(Device)
  end.

frequency_map(String) ->
  lists:foldl(fun add_occurrence/2, #{}, String).

add_occurrence(Char, Map) ->
  maps:put(Char, maps:get(Char, Map, 0) + 1, Map).

count_3_frequency_ids(Map_List) ->
  Maps_With3 = lists:filter(fun has_3_frequencies/1, Map_List),
  length(Maps_With3).

has_3_frequencies(Map) ->
  Map_With3 = maps:filter(fun is_3_frequency/2, Map),
  maps:size(Map_With3) > 0.

is_3_frequency(_Char, Frequency) ->
  Frequency == 3.

count_2_frequency_ids(Map_List) ->
  Maps_With2 = lists:filter(fun has_2_frequencies/1, Map_List),
  length(Maps_With2).

has_2_frequencies(Map) ->
  Map_With2 = maps:filter(fun is_2_frequency/2, Map),
  maps:size(Map_With2) > 0.

is_2_frequency(_Char, Frequency) ->
  Frequency == 2.

% Really bad solution
solution_2() ->
  Lines = readlines('input.txt'),
  Words = lists:map(fun string:trim/1, Lines),
  WordPairs = [{X,Y} || X <- Words, Y <- Words],
  NeededPair = search_solution(WordPairs),
  NeededPair.
  % Sorted = lists:sort(Lines1),
  % Reversed = lists:map(fun string:reverse/1, Lines1),
  % Sorted_Reversed = lists:sort(Reversed),
  % {Sorted, Sorted_Reversed}.

search_solution([]) ->
  throw("No solution found");
search_solution([{WordA, WordB} | Rest]) ->
  Position = position_of_difference_by_1_char(WordA, WordB),
  if 
    Position == 0 -> search_solution(Rest);
    true ->  without_nth_char(WordA, Position)
  end.

position_of_difference_by_1_char(WordA, WordB) ->
  Diffs = differences(WordA, WordB),
  if
    length(Diffs) == 1 -> [{_, _, Position} | _] = Diffs, Position;
    true -> 0
  end.

differences(WordA, WordB) ->
  Positions = lists:seq(1, length(WordA)),
  lists:filter(fun is_different/1, lists:zip3(WordA, WordB, Positions)).

is_different({Char_A, Char_B, _Position}) ->
  Char_A /= Char_B.

without_nth_char(String, Position) ->
  Index = Position - 1,
  Len = length(String),
  string:concat(string:slice(String, 0, Index), string:slice(String, Position, Len)).
