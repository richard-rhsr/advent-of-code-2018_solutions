-module(main).
-export([solution_1/0, solution_2/0
, input/0, test_input/0 , extra_test_inputs/0
, solve/1
, circular_repr/1
, solve_debug/1
, new_circular/0, move/2, insert_after/2, take_and_next/1
]).

solution_1() ->
  Input = input(),
  solve(Input).

solution_2() ->
  {Players, Marbles} = input(),
  solve({Players, Marbles * 100}).

input() ->
  [Line] = readlines('input.txt'),
  parse_line(Line).

test_input() ->
  {9, 25}.

extra_test_inputs() ->
  Lines = readlines('input2.txt'),
  lists:map(fun parse_line/1, Lines).

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
  io:format("Players: ~p, LastMarble: ~p~n", [PlayerNumber, LastMarble]),
  Circular = new_circular(),
  Marbles = lists:seq(1, LastMarble),
  Scores = empty_scores(PlayerNumber),
  Init = {Circular, Scores, 1},
  {_FinalPos, FinalScores, _} =  lists:foldl(fun make_move/2, Init, Marbles),
  max_score(FinalScores).

empty_scores(PlayerNumber) ->
  Players = lists:seq(1, PlayerNumber),
  maps:from_list(lists:zip(Players, lists:duplicate(PlayerNumber, 0))).

-record(circular, {first, last, current, nodes}).

new_circular() ->
  Nodes = #{0 => {start, finish}},
  Current = 0,
  First = Current,
  Last = Current,
  #circular{first=First, last=Last, current=Current, nodes=Nodes}.

move(0, #circular{} = Circular) -> Circular;
move(N, #circular{current=Current, last=Last, first=First} = Circular) when N > 0 andalso Current == Last ->
  move(N - 1, Circular#circular{current=First});
move(N, #circular{current=Current, nodes=Nodes} = Circular) when N > 0 ->
  {_Prev, Next} = maps:get(Current, Nodes),
  move(N - 1, Circular#circular{current=Next});
move(N, #circular{current=Current, last=Last, first=First} = Circular) when N < 0 andalso Current == First ->
  move(N + 1, Circular#circular{current=Last});
move(N, #circular{current=Current, nodes=Nodes} = Circular) when N < 0 ->
  {Prev, _Next} = maps:get(Current, Nodes),
  move(N + 1, Circular#circular{current=Prev}).

insert_after(New, #circular{current=Current, last=Last, nodes=Nodes} = Circular) when Current == Last ->
  {Prev, finish} = maps:get(Last, Nodes),
  Updates = #{Last => {Prev, New}, New => {Last, finish}},
  NewNodes = maps:merge(Nodes, Updates),
  Circular#circular{last=New, current=New, nodes=NewNodes};
insert_after(New, #circular{current=Current, nodes=Nodes} = Circular) ->
  {_Prev, Next} = maps:get(Current, Nodes),
  Nodes1 = maps:update_with(Next, fun ({_, Keep}) -> {New, Keep} end, Nodes),
  Nodes2 = maps:update_with(Current, fun ({Keep, _}) -> {Keep, New} end, Nodes1),
  NewNodes = maps:put(New, {Current, Next}, Nodes2),
  Circular#circular{nodes=NewNodes, current=New}.

take_and_next(#circular{current=Current, nodes=Nodes, last=Last, first=First} = Circular) when Current == Last ->
  {{Prev, finish}, Nodes1} = maps:take(Current, Nodes),
  NewNodes = maps:update_with(Prev, fun ({Keep, _}) -> {Keep, finish} end, Nodes1),
  {Current, Circular#circular{nodes=NewNodes, last=Prev, current=First}};
take_and_next(#circular{current=Current, nodes=Nodes} = Circular) ->
  {{Prev, Next}, Nodes1} = maps:take(Current, Nodes),
  Nodes2 = maps:update_with(Prev, fun ({Keep, _}) -> {Keep, Next} end, Nodes1),
  NewNodes = maps:update_with(Next, fun ({_, Keep}) -> {Prev, Keep} end, Nodes2),
  {Current, Circular#circular{nodes=NewNodes, current=Next}}.

make_move(Marble, {Circular, Scores, Player}) ->
  NextPlayer = next_player(Scores, Player),
  case Marble rem 23 of
    0 -> 
      {Extra, NewCircular} = take_and_next(move(-7, Circular)),
      NewScores = maps:update_with(Player, fun (Prev) -> Prev + Marble + Extra end, Scores),
      {NewCircular, NewScores, NextPlayer};
    _ -> 
      NewCircular = insert_after(Marble, move(1, Circular)),
      {NewCircular, Scores, NextPlayer}
  end.

next_player(Scores, Player) ->
  case Player == maps:size(Scores) of
    true -> 1;
    false -> Player + 1
  end.

max_score(Scores) ->
  lists:max(lists:map(fun ({_, Score}) -> Score end, maps:to_list(Scores))).

solve_debug({PlayerNumber, LastMarble}) ->
  io:format("Players: ~p, LastMarble: ~p~n", [PlayerNumber, LastMarble]),
  Circular = new_circular(),
  Marbles = lists:seq(1, LastMarble),
  Scores = empty_scores(PlayerNumber),
  Init = {Circular, Scores, 1},
  lists:foldl(
    fun (M, {C, S, P} = State) ->
      io:format("[~p] ~s  | ~p~n~n", [P, circular_repr(C), S]),
      make_move(M, State)
    end, 
    Init, 
    Marbles).

circular_repr(#circular{nodes=Nodes, current=Current, first=First}) ->
  NodesInOrder = lists:reverse(nodes_from(First, [], Nodes)),
  lists:flatten(string:join(
    lists:map(
      fun (Value) -> 
        case Value == Current of 
          true -> io_lib:format("(~p)", [Value]);
          false -> io_lib:format("~p", [Value])
        end
      end,
      NodesInOrder),
    " ")).

nodes_from(I, Acc, Nodes) ->
  case I of
    finish -> Acc;
    Index ->
      {_, Next} = maps:get(Index, Nodes),
      nodes_from(Next, [Index | Acc], Nodes)
  end.
