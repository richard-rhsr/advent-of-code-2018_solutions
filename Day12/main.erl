-module(main).
-export([solution_1/0, solution_2/0
, input/0, test_input/0
, solve_simple/2, solve_complex/2
, compare_solve/1
, generations_1/0, generations_2/0, generations_test_sample/0
]).

-define(BASE_PAD, 4).

solution_1() ->
  Input = input(),
  solve_simple(Input, generations_1()).

solution_2() ->
  Input = input(),
  solve_complex(Input, generations_2()).

compare_solve(Gen) ->
  Input = input(),
  {solve_simple(Input, Gen), solve_complex(Input, Gen)}.

generations_1() -> 20.
generations_2() -> 50 * 1000 * 1000 * 1000.
generations_test_sample() -> 200.

input() ->
  [Header, _Empty | Rules] = readlines('input.txt'),
  {
    parse_header(Header), 
    lists:map(fun parse_rule/1, Rules)
  }.

test_input() ->
  [Header, _Empty | Rules] = readlines('input2.txt'),
  {
    parse_header(Header), 
    lists:map(fun parse_rule/1, Rules)
  }.
  
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

parse_header(Line) ->
  [Chars] = string:lexemes(Line, "initial state:\n"),
  Chars.

parse_rule(Line) ->
  {ok, Regex} = re:compile("([#.]{5}) => ([#.])"),
  {match, [_, Pattern, Result]} = re:run(Line, Regex, [{capture, all, list}]),
  {Pattern, Result}.

compile_regex(PatternString) ->
  RegexPattern = lists:flatten(["(?=", string:replace(PatternString, ".", "\\.", all), ")"]),
  {ok, Regex} = re:compile(RegexPattern),
  Regex.

get_char([Char]) -> Char.

is_point($.) -> true;
is_point($#) -> false.

is_useful({_, [Char]}) -> not is_point(Char).

prepare_rules(RulesStrings) ->
  UsefulRules = lists:filter(fun is_useful/1, RulesStrings),
  lists:map(
    fun ({Pattern, Result}) ->
      {compile_regex(Pattern), get_char(Result)}
    end,
    UsefulRules).

-define(GENERATIONS, 500).

solve_simple({InitialBits, RulesStrings}, TargetGen) ->
  Rules = prepare_rules(RulesStrings),

  LeftPadLength = ?BASE_PAD,
  LeftPad = string:copies(".", LeftPadLength),
  RightPadLength = TargetGen + 2,
  RightPad = string:copies(".", RightPadLength),
  Buffer = lists:flatten([LeftPad, InitialBits, RightPad]),

  {ok, OutFile} = file:open("simple.log", [write]),
  print_gen(OutFile, 0, Buffer),
  FinalState = lists:foldl(
    fun (I, State) -> 
      NextGen = next_gen(State, Rules),
      print_gen(OutFile, I, NextGen),
      NextGen
    end, Buffer, lists:seq(1, TargetGen)),
  file:close(OutFile),
  Pots = filled_pots(FinalState, LeftPadLength),
  lists:sum(Pots).

next_gen(Buffer, Rules) ->
  Matches = lists:map(
    fun ({Re, Char}) -> 
      case re:run(Buffer, Re, [global]) of
        nomatch -> none;
        {match, Positions} ->
          lists:map(fun ([{I, _}]) -> {I+2, Char} end, Positions)
      end
    end, Rules),
  Updates = lists:flatten(lists:filter(
    fun (Match) -> 
      Match =/= none 
    end, Matches)),
  EmptyArray = array:new(length(Buffer), {default, $.}),
  FilledArray = lists:foldl(
    fun ({I, Char}, A) ->
      array:set(I, Char, A)
    end, EmptyArray, Updates),
  NewBuffer = array:to_list(FilledArray),
  NewBuffer.

filled_pots(Buffer, LeftOffset) ->
  IndexesPots = lists:zip(Buffer, lists:seq(1, length(Buffer))),
  FilledPots = lists:filter(fun ({C, _}) -> not is_point(C) end, IndexesPots),
  [Score - LeftOffset - 1 || {_, Score} <- FilledPots].

solve_complex({InitialBits, RulesStrings}, TargetGen) ->
  Rules = prepare_rules(RulesStrings),

  LeftPadLength = ?BASE_PAD,
  LeftPad = string:copies(".", LeftPadLength),
  RightPadLength = ?BASE_PAD,
  RightPad = string:copies(".", RightPadLength),
  Buffer = lists:flatten([LeftPad, InitialBits, RightPad]),

  RevCycle = get_cycle(Buffer, Rules),

  {ok, OutFile} = file:open("cycle.log", [write]),
  lists:foreach(fun ({I, Gen}) -> print_gen(OutFile, I, Gen) end, RevCycle),
  file:close(OutFile),
  
  [{_, FirstRepetition}, {LastUniqueGen, LastGenBuffer} | _] = RevCycle,
  if 
    TargetGen < LastUniqueGen ->
      lists:keyfind(TargetGen, 1, RevCycle);
    TargetGen >= LastUniqueGen ->
      FilledPots = filled_pots(LastGenBuffer, LeftPadLength),

      FirstFilledPotRepetition = length(lists:takewhile(fun is_point/1, FirstRepetition)),
      FirstFilledPotLastGen = length(lists:takewhile(fun is_point/1, LastGenBuffer)),
      Offset = FirstFilledPotRepetition - FirstFilledPotLastGen,
      
      (TargetGen - LastUniqueGen) * Offset * length(FilledPots) + lists:sum(FilledPots)
  end.
  
get_cycle(String, Rules) ->
  generate_cycles(Rules, [{0, String}]).
generate_cycles(Rules, Generations) ->
  [{N, String} | _] = Generations,
  Next = next_gen(String, Rules),
  NewGenerations = [{N + 1, adjust_right_padding(Next)} | Generations],
  case is_repetition(String, Next) of
    true -> NewGenerations;
    false -> generate_cycles(Rules, NewGenerations)
  end.

is_repetition(Previous, Current) ->
  clear_padding(Previous) == clear_padding(Current).

clear_padding(String) ->
  lists:reverse(lists:dropwhile(fun is_point/1, lists:reverse(lists:dropwhile(fun is_point/1, String)))).

adjust_right_padding(String) ->
  case lists:reverse(String) of
    [$., $., $., $. | _] -> String;
    [$., $., $. | _] -> lists:append(String, ".");
    [$., $. | _] -> lists:append(String, "..");
    [$. | _]  -> lists:append(String, "...");
    _ -> lists:append(String, "....")
  end.

print_gen(Device, N, State) ->
  io:format(Device, "[~3..\ B]: ~s~n", [N, State]).
