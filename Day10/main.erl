-module(main).
-export([solution_1/0, solution_2/0
, input/0, test_input/0
, solve/1
]).

solution_1() ->
  Input = input(),
  solve(Input).

solution_2() ->
  Input = input(),
  solve(Input).

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
  {ok, Regex} = re:compile("position=<\s*(-?\\d+),\s*(-?\\d+)> velocity=<\s*(-?\\d+),\s*(-?\\d+)>"),
  {match, [_, Px, Py, Vx, Vy]} = re:run(Line, Regex, [{capture, all, list}]),
  {{to_int(Px), to_int(Py)}, {to_int(Vx), to_int(Vy)}}.

to_int(String) -> 
  {Int, _} = string:to_integer(String),
  Int.

solve(InitialData) ->
  {InitPos, Vels, Spam} = init(InitialData),
  InitTime = 0,
  {FinalBoard, ElapsedTime} = find_minimum_spam({InitPos, Spam}, Vels, InitTime),
  io:format("ElapsedTime: ~p~n", [ElapsedTime]),
  print_message(FinalBoard).

init(InitialData) ->
  Positions = lists:map(fun ({P, _}) -> P end, InitialData),
  Velocities = lists:map(fun ({_, V}) -> V end, InitialData),
  {Positions, Velocities, calc_spam(Positions)}.

-record(spam, {xmin, xmax, ymin, ymax, width=0, height=0}).

calc_spam(Positions) ->
  Spam = lists:foldl(fun update_spam/2, undefined, Positions),
  #spam{xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax} = Spam,
  Spam#spam{width=(Xmax - Xmin + 1), height=(Ymax - Ymin + 1)}.

update_spam({X, Y}, undefined) -> 
  #spam{xmin=X, xmax=X, ymin=Y, ymax=Y};
update_spam({X, Y}, #spam{xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax}) -> 
  #spam{xmin=min(X, Xmin), xmax=max(X, Xmax), ymin=min(Y, Ymin), ymax=max(Y, Ymax)}.

find_minimum_spam({Positions, #spam{width=CurW, height=CurH}} = State, Velocities, ElapsedTime) ->
  UpdatedPos = lists:zipwith(fun update_position/2, Positions, Velocities),
  NewSpam = calc_spam(UpdatedPos),
  if
    CurW < NewSpam#spam.width orelse CurH < NewSpam#spam.height -> {State, ElapsedTime};
    true -> find_minimum_spam({UpdatedPos, NewSpam}, Velocities, ElapsedTime + 1)
  end.

update_position({Px, Py}, {Vx, Vy}) ->
  {Px + Vx, Py + Vy}.

print_message({Positions, #spam{width=W, xmin=X0, ymin=Y0, ymax=Y1}}) ->
  EmptyRow = array:new(W, {default, "."}),
  EmptyBoard = array:from_list(lists:map(fun (_) -> EmptyRow end, lists:seq(Y0, Y1))),
  FilledBoard = lists:foldl(
    fun ({X, Y}, Board) ->
      Line = array:get(Y-Y0, Board),
      array:set(Y-Y0, array:set(X-X0, "#", Line), Board)
    end, EmptyBoard, Positions),
  Lines = lists:map(fun (Line) -> lists:flatten(array:to_list(Line)) end, array:to_list(FilledBoard)),
  io:format("~s~n", [string:join(Lines, "\n")]).
