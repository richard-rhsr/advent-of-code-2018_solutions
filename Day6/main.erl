-module(main).
-export([solution_1/0, solution_2/0
, input/0, test_input/0
, solve_for_danger/1, solve_for_safefy/2, solve_faster/1
, calc_limits/1
, circle/2
, make_places/1
, filled_board/1
, biggest_area/1
, print_board/1
]).

-record(limits, {xmin=inifinte, xmax=0, ymin=inifinte, ymax=0}).
-record(cell, {dist=inifinte, id=undefined}).
-record(board, {limits, grid}).
-record(place, {id, x, y}).

-define(N,  { 0,  1}).
-define(S,  { 0, -1}).
-define(E,  { 1,  0}).
-define(W,  {-1,  0}).
-define(NE, { 1,  1}).
-define(NW, {-1,  1}).
-define(SE, { 1, -1}).
-define(SW, {-1, -1}).

solution_1() ->
  Coords = input(),
  solve_for_danger(Coords).

solution_2() ->
  Coords = test_input(),
  solve_for_safefy(Coords, 32).

input() ->
  Lines = readlines('input.txt'),
  lists:map(fun parse_line/1, Lines).

test_input() ->
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
  [X, Y] = string:lexemes(Line, ", \n"),
  {to_int(X), to_int(Y)}.

to_int(String) -> 
  {Int, _} = string:to_integer(String),
  Int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%%                                  SOLUTIONS                                 %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_for_danger(Coords) ->
  Places = make_places(Coords),
  Board = filled_board(Places),
  biggest_area(Board).

make_places(Coords) ->
  lists:zipwith(fun new_place/2, Coords, range_incl(1, length(Coords))).

filled_board(Places) ->
  EmptyBoard = empty_board(Places),  
  calc_cells(Places, EmptyBoard).

new_place({X, Y}, Id) ->
  #place{id=Id, x=X, y=Y}.

calc_limits(Places) ->
  Occupied = lists:foldl(fun update_limits/2, #limits{}, Places),
  #limits{
    xmin=(Occupied#limits.xmin - 1),
    xmax=(Occupied#limits.xmax + 1),
    ymin=(Occupied#limits.ymin - 1),
    ymax=(Occupied#limits.ymax + 1)
  }.

update_limits(#place{x=X, y=Y}, #limits{xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax}) ->
  #limits{
    xmin=min(X, Xmin), 
    xmax=max(X, Xmax), 
    ymin=min(Y, Ymin), 
    ymax=max(Y, Ymax)
  }.

empty_board(Places) ->
  Limits = calc_limits(Places),
  #limits{xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax} = Limits,
  Total = (Xmax - Xmin + 1) * (Ymax - Ymin + 1),
  #board{grid=array:new(Total, {default, #cell{}}), limits=Limits}.

calc_cells(Places, Board) ->
  lists:foldl(fun add_place/2, Board, Places).

add_place(#place{id=Id} = Place, #board{grid=Grid} = Board) ->
  I = get_index(Board, Place),
  NewGrid = array:set(I, #cell{id=Id, dist=0}, Grid),
  add_place(1, Place, Board#board{grid=NewGrid}).
add_place(L, #place{} = Place, #board{limits=Limits} = Board) ->
  IsInBoard = def_is_in_board(Limits),
  Positions = lists:filter(IsInBoard, circle(Place, L)),
  Updates = lists:filtermap(fun (Pos) -> filter_pos(L, Place, Board, Pos) end, Positions),
  if 
    length(Updates) == 0 -> Board;
    true -> add_place(L + 1, Place, lists:foldl(fun update_cell/2, Board, Updates))
  end.
  
filter_pos(L, #place{} = Place, #board{grid=Grid} = Board, {_, _} = Pos) ->
  Index = get_index(Board, Pos),
  Cell = array:get(Index, Grid),
  #cell{dist=D} = Cell,
  if
    D > L -> {true, {Pos, new_cell(L, Place)}};
    D == L -> {true, {Pos, undefined_cell(Cell)}};
    D < L -> false
  end.

new_cell(L, #place{id=Id}) ->
  #cell{dist=L, id=Id}.

undefined_cell(#cell{} = Current) ->
  Current#cell{id=undefined}.

range_incl(A, B) ->
  lists:seq(A, B).

update_cell({Pos, Cell}, Board) ->
  I = get_index(Board, Pos),
  NewGrid = array:set(I, Cell, Board#board.grid),
  Board#board{grid=NewGrid}.

mul(A, {X, Y}) -> {A * X, A * Y}.
add({X1, Y1}, {X2, Y2}) -> {X1 + X2, Y1 + Y2}.

circle(#place{x=X, y=Y}, L) ->
  Center = {X, Y},
  {N, E, S, W} = poles(Center, L),
  lists:flatten([
    side(N, ?SW, L),
    side(S, ?NE, L),
    side(E, ?NW, L),
    side(W, ?SE, L)
  ]).

poles({_, _} = Pos, Len) ->
  {add(mul(Len, ?N), Pos), 
   add(mul(Len, ?E), Pos), 
   add(mul(Len, ?S), Pos), 
   add(mul(Len, ?W), Pos)}.

side({_, _} = Pos, {_, _} = Dir, Len) ->
  Steps = range_to_0(Len),
  lists:map(fun (K) -> add(Pos, mul(K, Dir)) end, Steps).

range_to_0(N) -> 
  lists:seq(N - 1, 0, -1).

def_is_in_board(#limits{xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax}) ->
  fun ({X, Y}) ->
    (X >= Xmin) andalso (X =< Xmax) andalso (Y >= Ymin) andalso (Y =< Ymax)
  end.

get_index(#board{} = Board, #place{x=X, y=Y}) ->
  get_index(Board, {X, Y});
get_index(#board{limits=#limits{xmin=Xmin, xmax=Xmax, ymin=Ymin}}, {X, Y}) ->
  (Xmax - Xmin + 1) * (Y - Ymin) + (X - Xmin).

biggest_area(#board{} = Board) ->
  Stats = calc_areas(Board),
  BorderCells = border(Board),
  BorderIds = lists:map(fun (#cell{id=Id}) -> Id end, BorderCells),
  FobiddenIds = sets:add_element(undefined, sets:from_list(BorderIds)),
  IsInfinite = fun ({Id, _}) -> not sets:is_element(Id, FobiddenIds) end,
  FiniteAreas = lists:filter(IsInfinite, maps:to_list(Stats)),
  [{_, LargestArea} | _] = lists:sort(fun sort_by_area/2, FiniteAreas),
  LargestArea.

calc_areas(#board{grid=Grid}) ->
  array:foldl(fun add_area/3, maps:new(), Grid).

add_area(_, #cell{id = Id}, Map) ->
  Prev = maps:get(Id, Map, 0),
  maps:put(Id, Prev + 1, Map).

border(#board{limits=#limits{xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax}} = Board) ->
  Positions = lists:flatten([
    lists:map(fun (Y) -> {Xmin, Y} end, range_incl(Ymin, Ymax - 1)),
    lists:map(fun (Y) -> {Xmax, Y} end, range_incl(Ymin + 1, Ymax)),
    lists:map(fun (X) -> {X, Ymin} end, range_incl(Xmin + 1, Xmax)),
    lists:map(fun (X) -> {X, Ymax} end, range_incl(Xmin, Xmax - 1))
  ]),
  lists:map(fun (Pos) -> get_cell(Board, Pos) end, Positions).

sort_by_area({_, Area1}, {_, Area2}) ->
  Area1 > Area2.

get_cell(#board{} = Board, {_, _} = Pos) ->
  Index = get_index(Board, Pos),
  get_cell(Board, Index);
get_cell(#board{grid=Grid}, Index) ->
  array:get(Index, Grid).

solve_for_safefy(Coords, Threshold) ->
  Places = make_places(Coords),
  AllCoords = coords_board(Places),
  length(coords_under_limit(Places, AllCoords, Threshold)).

coords_board(Places) ->
  Limits = calc_limits(Places),
  #limits{xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax} = Limits,
  [{X, Y} || X <- range_incl(Xmin, Xmax), Y <- range_incl(Ymin, Ymax)].

coords_under_limit(Places, Coords, Threshold) ->
  lists:filter(fun (Coord) -> distance_to_all_places(Places, Coord) < Threshold end, Coords).

distance_to_all_places(Places, {X, Y}) ->
  Distances = lists:map(fun (#place{x=Xp, y=Yp}) -> abs(Xp - X) + abs(Yp - Y) end, Places),
  lists:sum(Distances).

solve_faster(Coords) ->
  Places = make_places(Coords),
  Board = fill_faster(Places),
  biggest_area(Board).

fill_faster(Places) ->
  AllCoords = coords_board(Places),
  Board = empty_board(Places),
  Updates = lists:map(
    fun (Pos) ->
      Cell = nearest_place(Places, Pos),
      {Pos, Cell}
    end, 
    AllCoords),
  lists:foldl(fun update_cell/2, Board, Updates).

nearest_place(Places, {X, Y}) ->
  DistanceToPoint = fun (#place{x=Xp, y=Yp}) -> abs(Xp - X) + abs(Yp - Y) end,
  Init = #cell{},
  lists:foldl(
    fun (#place{} = Place, #cell{dist=Current} = Cell) -> 
      Dist = DistanceToPoint(Place),
      if 
        Dist < Current -> new_cell(Dist, Place);
        Dist == Current -> undefined_cell(Cell);
        Dist > Current -> Cell
      end
    end, Init, Places).


print_board(#board{limits=#limits{ymax=Ymax, ymin=Ymin}} = Board) -> 
  Range = range_incl(Ymin, Ymax),
  Lines = lists:map(fun (Y) -> line_repr(Board, Y) end, Range),
  String = string:join(Lines, "\n"),
  io:format("~s~n", [String]).

line_repr(#board{limits=#limits{xmax=Xmax, xmin=Xmin}} = Board, Y) ->
  Start = get_index(Board, {Xmin, Y}),
  End = get_index(Board, {Xmax, Y}),
  Range = range_incl(Start, End),
  Repr = lists:map(fun (I) -> get_cell_repr(Board, I) end, Range),
  lists:flatten(Repr).

get_cell_repr(#board{}=Board, Index) ->
  #cell{id=Id} = get_cell(Board, Index),
  id_repr(Id).

id_repr(undefined) -> ".";
id_repr(Id) -> 
  Chars = array:from_list(lists:flatten([
    range_incl($a, $z), 
    range_incl($A, $Z),
    range_incl($0, $9)
  ])),
  array:get(Id, Chars).