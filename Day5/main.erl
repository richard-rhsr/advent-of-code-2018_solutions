-module(main).
-export([solution_1/0, solution_2/0, solve/1, solve_slow/1, input/0, test_input/0]).

input() -> 
  [Line] = readlines('input.txt'),
  Line.

test_input() -> 
  [Line] = readlines('input2.txt'),
  Line.

solution_1() ->
  Input = input(),
  solve(Input).

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

solution_2() ->
  Input = input(),
  Types = sets:to_list(sets:from_list(lists:map(fun (C) -> string:to_lower(C) end, Input))),
  ImprovedAttempts = lists:map(fun (C) -> remove_type(Input, C) end, Types),
  ImprovedInertLengths = lists:map(fun solve/1, ImprovedAttempts),
  lists:min(ImprovedInertLengths).

remove_type(Polymer, Char) ->
  Lower = string:to_lower(Char),
  Upper = string:to_upper(Char),
  Without = string:lexemes(Polymer, [Lower, Upper]),
  lists:flatten(Without).

solve(Polymer) ->
  Inert = fully_react(Polymer),
  length(Inert).

init(Polymer) ->
  Stack = [],
  P1 = 1,
  P2 = case length(Polymer) > 1 of true -> 2; false -> 0 end,
  {build_tree(Polymer), Stack, P1, P2}.

build_tree(String) ->
  Indexes = lists:seq(1, length(String)),
  Entries = lists:zip(Indexes, String),
  OrdDict = orddict:from_list(Entries),
  gb_trees:from_orddict(OrdDict).

retrieve_string(Tree) ->
  Entries = gb_trees:to_list(Tree),
  lists:map(fun ({_, C}) -> C end, Entries).

fully_react(Polymer) when erlang:is_list(Polymer) -> 
  fully_react(init(Polymer));
fully_react({Polymer, _Stack, _P1, 0}) -> 
  retrieve_string(Polymer);
fully_react({Polymer, Stack, P1, P2} = Current) ->
  Cond = can_react(Current),
  if
    Cond -> fully_react(react({Polymer, Stack, P1, P2}));
    true -> fully_react(advance({Polymer, Stack, P1, P2}))
  end.

can_react({Polymer, _, P1, P2}) ->
  First = gb_trees:get(P1, Polymer),
  Second = gb_trees:get(P2, Polymer),
  Case1 = string:to_lower(First) == Second andalso string:to_upper(Second) == First,
  Case2 = string:to_upper(First) == Second andalso string:to_lower(Second) == First,
  Case1 orelse Case2.

react({Polymer, Stack, P1, P2}) ->
  {_, Rem1} = gb_trees:take(P1, Polymer),
  {_, Remaining} = gb_trees:take(P2, Rem1),
  rewind({Remaining, Stack, P1, P2}).

rewind({Polymer, [], _, _}) -> 
  {P1, P2} = find_2_indexes(Polymer),
  {Polymer, [], P1, P2};
rewind({Polymer, Stack, _P1, P2}) ->
  [NewP1 | NewStack] = Stack,
  Iterator = gb_trees:iterator_from(P2, Polymer),
  {NewP2, _} = find_1_index(Iterator),
  {Polymer, NewStack, NewP1, NewP2}.

find_2_indexes(Polymer) ->
  I1 = gb_trees:iterator(Polymer),
  {P1, I2} = find_1_index(I1),
  case I2 of
    none -> {P1, 0};
    _ -> 
      {P2, _} = find_1_index(I2),
      {P1, P2}
  end.

find_1_index(Iterator) ->
  case gb_trees:next(Iterator) of
    none -> {0, none};
    {K, _, Next} -> {K, Next}
  end.

advance({Polymer, Stack, P1, P2}) ->
  Iterator = gb_trees:iterator_from(P2 + 1, Polymer),
  case gb_trees:next(Iterator) of
    none -> {Polymer, Stack, P1, 0};
    {NewP2, _, _} -> 
      NewP1 = P2,
      NewStack = [P1 | Stack],
      {Polymer, NewStack, NewP1, NewP2}
  end.

solve_slow(Polymer) ->
  fully_react_slow(Polymer).

% slow solution, probably n² because of slices with mid-string deletion
fully_react_slow(Polymer) when erlang:is_list(Polymer) -> 
  fully_react_slow({Polymer, 1});
fully_react_slow({Polymer, Position}) when Position >= erlang:length(Polymer) -> 
  length(Polymer);
fully_react_slow({Polymer, Position}) ->
  First = lists:nth(Position, Polymer),
  Second = lists:nth(Position + 1, Polymer),
  Case1 = string:to_lower(First) == Second andalso string:to_upper(Second) == First,
  Case2 = string:to_upper(First) == Second andalso string:to_lower(Second) == First,
  if
    Case1 orelse Case2 -> react_slow({Polymer, Position});
    true -> fully_react_slow({Polymer, Position + 1})
  end.

react_slow({String, Position}) ->
  Index = Position - 1,
  Len = length(String),
  Remaining = string:concat(
    string:slice(String, 0, Index), 
    string:slice(String, Index + 2, Len)
  ),
  fully_react_slow({Remaining, max(Position - 1, 1)}).


print_tree(Tree) ->
  io:format("~p", [gb_trees:to_list(Tree)]).

print_iter(none) -> ok;
print_iter([]) -> io:format(" ITERATOR.", []);
print_iter(Iterator) ->
  {K, V, Rem} = gb_trees:next(Iterator),
  io:format(" ~p: ~p |", [K, V]),
  print_iter(Rem).
