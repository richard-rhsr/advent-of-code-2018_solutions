-module(main).
-export([solution_1/0, solution_2/0
, input/0, test_input1/0, test_input2/0
, solve_3x3/1, solve_NxN/1
]).

solution_1() ->
  Input = input(),
  solve_3x3(Input).

solution_2() ->
  Input = input(),
  solve_NxN(Input).

input() -> 5468.

test_input1() -> 18.

test_input2() -> 42.

solve_3x3(SerialNumber) ->
  Grid = cells(SerialNumber),
  Candidates = [{X, Y} || X <- lists:seq(1, 298), Y <- lists:seq(1, 298)],
  Sums = lists:map(fun (Coord) -> {Coord, subgrid_3x3_sum(Grid, Coord)} end, Candidates),
  lists:foldl(fun larger_sum/2, {none, -100}, Sums).

cells(SerialNumber) ->
  Entries = [{{X, Y}, level(X, Y, SerialNumber)} || X <- lists:seq(1, 300), Y <- lists:seq(1, 300)],
  maps:from_list(Entries).

level(X, Y, SerialNumber) ->
  RackId = X + 10,
  BaseLevel = (RackId * Y + SerialNumber) * RackId,
  HundredsDigit = (BaseLevel div 100) rem 10,
  HundredsDigit - 5.

subgrid_3x3_sum(Grid, {Xi, Yi}) ->
  Cells = [{X, Y} || X <- [Xi, Xi+1, Xi+2], Y <- [Yi, Yi+1, Yi+2]],
  lists:sum(lists:map(fun (Coord) -> maps:get(Coord, Grid) end, Cells)).

larger_sum({_, Level} = New, {_, BiggestSum}) when Level > BiggestSum -> New;
larger_sum({_, _}, {_, _} = Current) -> Current.

% solve_NxN(SerialNumber) ->
%   Grid = cells(SerialNumber),
%   Candidates = [{X, Y, S} || S <- lists:seq(2, 300), X <- lists:seq(1, 300 - S + 1), Y <- lists:seq(1, 300 - S + 1)],
%   Sums = lists:map(fun (Coord) -> {Coord, subgrid_NxN_sum(Grid, Coord)} end, Candidates),
%   lists:foldl(fun larger_sum/2, {none, -100000000}, Sums).

% subgrid_NxN_sums(Grid, {Xi, Yi}, Size) ->
%   Cells = [{X, Y} || X <- lists:seq(Xi, Xi + Size - 1), Y <- lists:seq(Yi, Yi + Size - 1)],
%   lists:sum(lists:map(fun (Coord) -> maps:get(Coord, Grid) end, Cells)).

solve_NxN(SerialNumber) ->
  Grid = cells(SerialNumber),
  Sizes = lists:seq(4, 300),
  SizesAndSums = lists:map(fun (S) -> 
    io:format("Calculando agora quadrados de lado ~p~n", [S]),
    largest_subgrid_NxN_sum(Grid, S)
  end, Sizes),
  lists:foldl(fun larger_sum/2, {none, -100000000}, SizesAndSums).

largest_subgrid_NxN_sum(Grid, Size) ->
  AllSeq = lists:seq(1, 300),
  SizeSeq = lists:seq(1, 300 - Size + 1),
  PartialCoords = [{X, Y} || X <- SizeSeq, Y <- AllSeq],
  PartialSumsEntries = lists:map(fun ({Xi, Y}) ->
    Columns = lists:seq(Xi, Xi + Size - 1),
    Sum = lists:sum(lists:map(fun (Col) -> maps:get({Col, Y}, Grid) end, Columns)),
    {{Xi, Y}, Sum}
  end, PartialCoords),
  PartialSums = maps:from_list(PartialSumsEntries),
  Candidates = [{X, Y} || X <- SizeSeq, Y <- SizeSeq],
  Sums = lists:map(fun ({X, Yi}) -> 
    Rows = lists:seq(Yi, Yi + Size - 1),
    Sum = lists:sum(lists:map(fun (Row) -> maps:get({X, Row}, PartialSums) end, Rows)),
    {{X, Yi, Size}, Sum}
  end, Candidates),
  lists:foldl(fun larger_sum/2, {none, -100000000}, Sums).
