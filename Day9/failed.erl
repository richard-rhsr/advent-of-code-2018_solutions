-module(failed).
-export([solution_1/0, solution_2/0
, input/0, test_input/0
, dummy_input/0
, first_problemtic_input/0
, solve/1
, new_circular/1, insert/2
, circular_repr/1
, find_eps/0
]).

-define(has_size1_gb_tree(S), (is_tuple(S) andalso size(S) == 2 andalso element(1, S) == 1)).
% has_size1_gb_tree({1, _}) -> true;
% has_size1_gb_tree({S, _}) -> false.

-record(circular, {top, forward, backward, current}).

solution_1() ->
  [Input] = input(),
  solve(Input).

solution_2() ->
  [{Players, Marbles}] = input(),
  solve({Players, Marbles * 100}).

input() ->
  Lines = readlines('input.txt'),
  lists:map(fun parse_line/1, Lines).

test_input() ->
  Lines = readlines('input2.txt'),
  lists:map(fun parse_line/1, Lines).

dummy_input() ->
  [{9, 25}].

first_problemtic_input() ->
  {458, 4194304}.

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

parse_line(Line) ->
  {ok, Regex} = re:compile("(\\d+) players; last marble is worth (\\d+) points"),
  {match, [_, PlayerNumber, LastMarble]} = re:run(Line, Regex, [{capture, all, list}]),
  {to_int(PlayerNumber), to_int(LastMarble)}.

to_int(String) -> 
  {Int, _} = string:to_integer(String),
  Int.

solve({PlayerNumber, LastMarble}) ->
  io:format("~p~n", [LastMarble]),
  Circular = new_circular(LastMarble),
  Marbles = lists:seq(1, LastMarble),
  Scores = empty_scores(PlayerNumber),
  Init = {Circular, Scores, 1},
  {_FinalPos, FinalScores, _} =  lists:foldl(fun make_move/2, Init, Marbles),
  max_score(FinalScores).
  % lists:foldl(fun (M, {C, S, P} = State) ->
  %   case M of
  %     4194304 ->
  %       io:format("[~p] ~p~n", [C#circular.current, gb_trees:to_list(C#circular.forward)]);
  %     _ -> do_nothing
  %   end,
  %   % io:format("[~p] ~s  | ~p~n", [P, circular_repr(C), S]),
  %   make_move(M, State)
  % end, Init, Marbles).

empty_scores(PlayerNumber) ->
  Players = lists:seq(1, PlayerNumber),
  maps:from_list(lists:zip(Players, lists:duplicate(PlayerNumber, 0))).

new_circular(Top) ->
  EmptyTree = gb_trees:empty(),
  Current = 1,
  Forward = gb_trees:insert(Current, 0, EmptyTree),
  Backward = gb_trees:insert(-Current, 0, EmptyTree),
  #circular{top=Top, forward=Forward, backward=Backward, current=Current}.

insert(N, #circular{forward=Forward} = Circular)
 when ?has_size1_gb_tree(Forward) -> insert_at_end(N, Circular);
insert(N, #circular{forward=Forward, current=Current} = Circular) ->
  FromCurrent = gb_trees:iterator_from(Current, Forward),
  {_, _, Current_plus_1} = gb_trees:next(FromCurrent),
  case gb_trees:next(Current_plus_1) of 
    none -> 
      ItBegin = gb_trees:iterator(Forward),
      {Index1, _, ItBegin_1} = gb_trees:next(ItBegin),
      {Index2, _, _} = gb_trees:next(ItBegin_1),
      insert_between(Index1, Index2, N, Circular);
    {Index1, _, NextIt} ->
      case gb_trees:next(NextIt) of
        none -> insert_at_end(N, Circular);
        {Index2, _, _} -> insert_between(Index1, Index2, N, Circular)
      end
  end.

insert_at_end(N, #circular{forward=Forward} = Circular) ->
  {Largest, _} = gb_trees:largest(Forward),
  NewIndex = Largest + 1,
  perform_insert(NewIndex, N, Circular).

insert_between(Index1, Index2, N, #circular{} = Circular) ->
  NewIndex = (Index1 + Index2) / 2,
  perform_insert(NewIndex, N, Circular).

perform_insert(NewIndex, N, #circular{forward=Forward, backward=Backward} = Circular) ->
  NewForward = gb_trees:insert(NewIndex, N, Forward),
  NewBackward = gb_trees:insert(-NewIndex, N, Backward),
  Circular#circular{forward=NewForward, backward=NewBackward, current=NewIndex}.

take(0, #circular{forward=Forward, backward=Backward, current=Current} = Circular) ->
  NewIndex = next_index(Circular),
  {Value, NewForward} = gb_trees:take(Current, Forward),
  {_, NewBackward} = gb_trees:take(-Current, Backward),
  {Value, Circular#circular{current=NewIndex, forward=NewForward, backward=NewBackward}};
take(N, #circular{forward=Forward, current=Current} = Circular) when N > 0 ->
  ForwardIt = gb_trees:iterator_from(Current, Forward),
  {_, _, Right1_It} = gb_trees:next(ForwardIt),
  Right1_Index = case gb_trees:next(Right1_It) of
    none -> 
      {Index, _} = gb_trees:smallest(Forward),
      Index;
    {Index, _, _} -> 
      Index
  end,
  take(N - 1, Circular#circular{current = Right1_Index});
take(N, #circular{backward=Backward, current=Current} = Circular) when N < 0 ->
  BackwardIt = gb_trees:iterator_from(-Current, Backward),
  {_, _, Left1_It} = gb_trees:next(BackwardIt),
  Left1_Index = case gb_trees:next(Left1_It) of
    none -> 
      {Index, _} = gb_trees:largest(Backward),
      Index;
    {Index, _, _} -> 
      Index
  end,
  take(N + 1, Circular#circular{current = -Left1_Index}).

next_index(#circular{forward=Forward, current=Current}) ->
  ForwardIt = gb_trees:iterator_from(Current, Forward),
  {_, _, Right1_It} = gb_trees:next(ForwardIt),
  case gb_trees:next(Right1_It) of
    none -> 
      {Index, _} = gb_trees:smallest(Forward),
      Index;
    {Index, _, _} -> Index
  end.

make_move(Marble, {Circular, Scores, Player}) ->
  NextPlayer = next_player(Scores, Player),
  case Marble rem 23 of
    0 -> 
      {Extra, NewCircular} = take(-7, Circular),
      NewScores = maps:update_with(Player, fun (Prev) -> Prev + Marble + Extra end, Scores),
      {NewCircular, NewScores, NextPlayer};
    _ -> 
      NewCircular = insert(Marble, Circular),
      {NewCircular, Scores, NextPlayer}
  end.

next_player(Scores, Player) ->
  case Player == maps:size(Scores) of
    true -> 1;
    false -> Player + 1
  end.

max_score(Scores) ->
  lists:max(lists:map(fun ({_, Score}) -> Score end, maps:to_list(Scores))).

circular_repr(#circular{forward=Forward, current=Current}) ->
  lists:flatten(string:join(
    lists:map(
      fun ({Index, Value}) -> 
        case Index == Current of 
          true -> io_lib:format("(~p)", [Value]);
          false -> io_lib:format("~p", [Value])
        end
      end,
      gb_trees:to_list(Forward)),
    " ")).

find_eps() ->
  find_eps(1).
find_eps(N) ->
  N2 = N / 2,
  if 
    0.0 == N2 -> N;
    true -> find_eps(N2)
  end.
