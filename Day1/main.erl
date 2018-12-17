-module(main).
-export([solution_1/0, solution_2/0]).

solution_1() ->
  Lines = readlines('input.txt'),
  Integers = lists:map(fun to_int/1, Lines),
  Result = lists:foldl(fun sum/2, 0, Integers),
  Result.

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

to_int(String) -> 
  {Int, _} = string:to_integer(String),
  Int.

sum(X, Y) ->
  X + Y.

solution_2() ->
  Lines = readlines('input.txt'),
  Integers = lists:map(fun to_int/1, Lines),
  Init = {0, sets:from_list([0]), not_seen},
  fold_until(Init, new_circular_list(Integers)).

fold_until(State, CircularList) ->
  {NewCircularList, Elem} = get_next(CircularList),
  New_State = update_state(Elem, State),
  case New_State of
    {Current, _, seen} -> Current;
    {_, _, not_seen} -> fold_until(New_State, NewCircularList)
  end.

update_state(Elem, {Current, Set, not_seen}) ->
  New_Elem = sum(Current, Elem),
  case sets:is_element(New_Elem, Set) of
    true -> {New_Elem, Set, seen};
    false -> {New_Elem, sets:add_element(New_Elem, Set), not_seen}
  end.

get_next(CircularList) -> 
  {List, Index} = CircularList,
  New_Index = (Index + 1) rem array:size(List),
  Elem = array:get(Index, List),
  {{List, New_Index}, Elem}.

new_circular_list(List) ->
  {array:from_list(List), 0}.
