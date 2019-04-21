-module(alt).
-export([solution_1/0
, input/0, test_input1/0, test_input2/0, test_input3/0, test_input4/0
, start_scores/0
, solve_count/2
, print_state/2
]).

-define(EXTRA_RECIPES, 10).

% Using map as data structure did not work for achieving better runnning time:
%   Array based solution to first part is ~1.3s
%   Map based solution to first part is ~2.4s

solution_1() ->
  Input = input(),
  solve_count(Input, start_scores()).

input() -> 864801.

test_input1() -> 9.

test_input2() -> 5.

test_input3() -> 18.

test_input4() -> 2018.

start_scores() ->
  [3, 7].

solve_count(TargetRecipe, StartingRecipes) ->
  StartingBuffer = insert_at_tail({#{}, 0}, StartingRecipes),
  Indexes = lists:seq(0, length(StartingRecipes) - 1),
  TargetSize = TargetRecipe + ?EXTRA_RECIPES + 2, % in the case a number with 2 digits needs to be added
  {Map, _} = fill_recipes(TargetSize, StartingBuffer, Indexes),
  ExtraRecipes = lists:map(fun (I) -> maps:get(I, Map) end, lists:seq(TargetRecipe, TargetRecipe + 10 - 1)),
  from_digits(ExtraRecipes).
  
insert_at_tail({_, _} = Buffer, []) -> Buffer;
insert_at_tail({Map, Size}, [Score | RemainingScores]) ->
  insert_at_tail({maps:put(Size, Score, Map), Size + 1}, RemainingScores).

to_digits(N) ->
  lists:map(fun (C) -> C - $0 end, integer_to_list(N)).

fill_recipes(LastRecipe, {_, Count} = Buffer, _) when Count >= LastRecipe -> Buffer;
fill_recipes(LastRecipe, {Recipes, _} = Buffer, Indexes) ->
  CurrentRecipes = lists:map(fun (I) -> maps:get(I, Recipes) end, Indexes),
  NewScores = to_digits(lists:sum(CurrentRecipes)),
  NewBuffer = insert_at_tail(Buffer, NewScores),
  {_, NewCount} = NewBuffer,
  NewIndexes = lists:zipwith(fun (I, S) -> (I + S + 1) rem NewCount end, Indexes, CurrentRecipes),
  fill_recipes(LastRecipe, NewBuffer, NewIndexes).

from_digits(Digits) ->
  lists:map(fun (D) -> D + $0 end, Digits).

print_state({Map, _}, [I0, I1]) ->
  OrdDict = maps:to_list(Map),
  Strings = lists:map(
    fun ({I, Score}) ->
      if
        I == I0 -> io_lib:format("(~p)", [Score]);
        I == I1 -> io_lib:format("[~p]", [Score]);
        true -> io_lib:format(" ~p ", [Score])
      end
    end, OrdDict),
  io:format("~s~n", [lists:flatten(Strings)]).
