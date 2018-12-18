-module(main).
-include_lib("stdlib/include/assert.hrl").
-export([solution_1/0, solution_2/0]).
-record(claim, {left, right, top, bottom, area, id}).
-define(SIZE, 1000).

parse_claim(String) ->
  [IdT, LeftT, TopT, WidthT, HeightT] = string:lexemes(String, "# @,:x\n"),
  {Id, _} = string:to_integer(IdT),
  {Left, _} = string:to_integer(LeftT),
  {Top, _} = string:to_integer(TopT),
  {Width, _} = string:to_integer(WidthT),
  {Height, _} = string:to_integer(HeightT),
  Area = Width * Height,
  Right = Left + Width,
  Bottom = Top + Height,
  #claim{left=Left, right=Right, top=Top, bottom=Bottom, area=Area, id=Id}.

solution_1() ->
  Lines = readlines('input.txt'),
  Claims = lists:map(fun parse_claim/1, Lines),
  Init = init(),
  FabricWithClaims = lists:foldl(fun superpose/2, Init, Claims),
  OverUsedCells = maps:filter(fun is_within_2_or_more_claims/2, FabricWithClaims),
  maps:size(OverUsedCells).

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

range(Min, Max_excluded) ->
  lists:seq(Min, Max_excluded - 1).

cells(#claim{left=L, right=R, top=T, bottom=B}) ->
  ?assert (R > L, "Right should be strictly greater than Left"),
  ?assert (B > T, "Top should be strictly greater than Bottom"),
  H_Indexes = range(L, R),
  V_Indexes = range(T, B),
  [{X,Y} || X <- H_Indexes, Y <- V_Indexes].

init() ->
  #{}.

superpose(Claim, Fabric) ->
  Cells = cells(Claim),
  lists:foldl(fun fill_cell/2, Fabric, Cells).

fill_cell(Cell, Fabric) ->
  maps:update_with(Cell, fun inc/1, 1, Fabric).

inc (X) -> X + 1.

is_within_2_or_more_claims(_Pair, Count) ->
  Count > 1.

solution_2() ->
  Lines = readlines('input.txt'),
  Claims = lists:map(fun parse_claim/1, Lines),
  Init = init2(Claims),
  {_Fabric, Intact} = lists:foldl(fun superpose2/2, Init, Claims),
  lists:flatmap(fun erlang:integer_to_list/1, sets:to_list(Intact)).

init2(Claims) ->
  Entries = lists:map(fun id/1, Claims),
  {init(), sets:from_list(Entries)}.

id(#claim{id=Id}) -> Id.

superpose2(Claim, State) ->
  Cells = cells_with_claim(Claim),
  lists:foldl(fun update_cell/2, State, Cells).

cells_with_claim(Claim) ->
  #claim{id=Id} = Claim,
  Cells = cells(Claim),
  lists:map(fun (Cell) -> {Cell, Id} end, Cells).

update_cell({Cell, Id}, {Fabric, Intact}) ->
  case maps:get(Cell, Fabric, none) of
    none -> 
      NewFabric = maps:put(Cell, Id, Fabric), 
      {NewFabric, Intact};
    OtherId -> 
      NewIntact = sets:del_element(OtherId, sets:del_element(Id, Intact)), 
      {Fabric, NewIntact}
  end.

calc_delimiters(Claims) ->
  Init = init_delimiters(),
  {H_Delimiters, V_Delimiters} = lists:foldl(fun slice_regions/2, Init, Claims),
  {ordsets:size(H_Delimiters), ordsets:size(V_Delimiters)}.

init_delimiters() -> 
  {ordsets:new(), ordsets:new()}.

slice_regions(#claim{left=L, right=R, top=T, bottom=B}, {Hset, Vset}) ->
  New_Hset = ordsets:add_element(L, ordsets:add_element(R, Hset)),
  New_Vset = ordsets:add_element(T, ordsets:add_element(B, Vset)),
  {New_Hset, New_Vset}.
