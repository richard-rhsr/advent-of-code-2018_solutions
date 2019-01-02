-module(main).
-export([solution_1/0, solution_2/0
, input_p1/0, input_p2/0
, test1_input_p1/0, test1_input_p2/0
, test2_input_p1/0, test2_input_p2/0
, test3_input_p1/0, test3_input_p2/0
, test4_input_p1/0, test4_input_p2/0
, start_scores/0
, solve_count/2
, solve_pattern/2
, print_state/2
]).

-define(EXTRA_RECIPES, 10).

solution_1() ->
  Input = input_p1(),
  solve_count(Input, start_scores()).

solution_2() ->
  Input = input_p2(),
  solve_pattern(to_digits(Input), start_scores()).

input_p1() -> 864801.
input_p2() -> integer_to_list(input_p1()).

test1_input_p1() -> 9.
test1_input_p2() -> "51589".

test2_input_p1() -> 5.
test2_input_p2() -> "01245".

test3_input_p1() -> 18.
test3_input_p2() -> "92510".

test4_input_p1() -> 2018.
test4_input_p2() -> "59414".

start_scores() ->
  [3, 7].

solve_count(TargetRecipe, StartingRecipes) ->
  ArraySize = TargetRecipe + ?EXTRA_RECIPES + 2, % in the case a number with 2 digits needs to be added
  StartingBoard = insert_at_tail({array:new(ArraySize), 0}, StartingRecipes),
  Indexes = lists:seq(0, length(StartingRecipes) - 1),
  {Array, _} = fill_recipes_up_to(ArraySize, {StartingBoard, Indexes}),
  ExtraRecipes = lists:map(fun (I) -> array:get(I, Array) end, lists:seq(TargetRecipe, TargetRecipe + 10 - 1)),
  from_digits(ExtraRecipes).
  
insert_at_tail({_, _} = Scoreboard, []) -> Scoreboard;
insert_at_tail({Array, Size}, [Score | RemainingScores]) ->
  insert_at_tail({array:set(Size, Score, Array), Size + 1}, RemainingScores).

to_digits(NumericString) when is_list(NumericString) ->
  lists:map(fun (C) -> C - $0 end, NumericString);
to_digits(N) ->
  to_digits(integer_to_list(N)).

fill_recipes_up_to(LastRecipe, {{_, Count} = Scoreboard, _}) when Count >= LastRecipe -> Scoreboard;
fill_recipes_up_to(LastRecipe, {{_, _}, _} = State) ->
  NewState = add_recipes(State),
  fill_recipes_up_to(LastRecipe, NewState).

add_recipes({{Recipes, _} = Scoreboard, Indexes}) ->
  CurrentRecipes = lists:map(fun (I) -> array:get(I, Recipes) end, Indexes),
  NewScores = to_digits(lists:sum(CurrentRecipes)),
  NewScoreboard = insert_at_tail(Scoreboard, NewScores),
  {_, NewCount} = NewScoreboard,
  NewIndexes = lists:zipwith(fun (I, S) -> (I + S + 1) rem NewCount end, Indexes, CurrentRecipes),
  {NewScoreboard, NewIndexes}.

from_digits(Digits) ->
  lists:map(fun (D) -> D + $0 end, Digits).

solve_pattern(Pattern, StartingRecipes) ->
  StartingBoard = insert_at_tail({array:new(), 0}, StartingRecipes),
  Indexes = lists:seq(0, length(StartingRecipes) - 1),
  {_State, N} = fill_until_match_pattern(Pattern, {StartingBoard, Indexes}),
  N.

fill_until_match_pattern(Pattern, {{_, Count}, _} = State) ->
  NewState = add_recipes(State),
  {{_, NewCount} = NewScoreboard, _} = NewState,
  case check_pattern_last_N(Pattern, NewScoreboard, NewCount - Count) of
    false -> fill_until_match_pattern(Pattern, NewState);
    N -> {NewState, N}
  end.

check_pattern_last_N(Pattern, {Recipes, Size}, N) ->
  Len = length(Pattern),
  Ends = lists:seq(Size - N, Size - 1),
  Limits = lists:map(fun (End) -> {End - Len + 1, End} end, Ends),
  AnySublistMatches = lists:search(fun(Limit) -> 
    check_pattern(Pattern, Recipes, Limit)
    end, Limits),
  case AnySublistMatches of
    false -> false;
    {value, {Start, _}} -> Start
  end.
  
check_pattern(_, _, {First, _}) when First < 0 -> false;
check_pattern(Pattern, Array, {First, Last}) ->
  Candidate = lists:map(fun (I) -> array:get(I, Array) end, lists:seq(First, Last)),
  Candidate == Pattern.

print_state({Array, _}, [I0, I1]) ->
  OrdDict = array:sparse_to_orddict(Array),
  Strings = lists:map(
    fun ({I, Score}) ->
      if
        I == I0 -> io_lib:format("(~p)", [Score]);
        I == I1 -> io_lib:format("[~p]", [Score]);
        true -> io_lib:format(" ~p ", [Score])
      end
    end, OrdDict),
  io:format("~s~n", [lists:flatten(Strings)]).
