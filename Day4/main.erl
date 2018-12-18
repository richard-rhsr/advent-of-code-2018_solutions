-module(main).
-include_lib("stdlib/include/assert.hrl").
-export([solution_1/0, solution_2/0]).

solution_1() ->
  Lines = readlines('input.txt'),
  Ordered = lists:sort(Lines),
  Records = lists:map(fun parse_record/1, Ordered),
  ShiftLogs = partition_per_shift(Records),
  ShiftsSummary = lists:foldl(fun summarize_intervals/2, #{}, ShiftLogs),
  get_most_sleepy(ShiftsSummary).

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

parse_record(String) ->
  Tokens = string:lexemes(String, " []#\n"),
  [_Date, Time, First, Second | _] = Tokens,
  case First of
    "Guard" -> 
      Id = to_int(Second), 
      {shift, Id};
    "falls" -> 
      ?assertEqual(Second, "asleep"), 
      {_, Minute} = parseTime(Time),
      {sleeps, Minute};
    "wakes" -> 
      ?assertEqual(Second, "up"), 
      {_, Minute} = parseTime(Time),
      {awakes, Minute}
  end.

to_int(String) -> 
  {Int, _} = string:to_integer(String),
  Int.

parseTime(String) ->
  [Hour, Minute] = string:lexemes(String, ":"),
  {to_int(Hour), to_int(Minute)}.

partition_per_shift([{shift, Id} | Rest]) ->
  {Actions, Tail} = lists:splitwith(fun is_not_shift/1, Rest),
  [{Id, group_actions(Actions)} | partition_per_shift(Tail)];
partition_per_shift([]) -> [].

is_not_shift({shift, _}) -> false;
is_not_shift({_, _}) -> true.

group_actions([{sleeps, Start}, {awakes, End} | Rest]) ->
  [range(Start, End) | group_actions(Rest)];
group_actions([]) -> [].

range(Min, Max_excluded) ->
  lists:seq(Min, Max_excluded - 1).

summarize_intervals({Id, SleepIntervals}, ShiftsSummary) ->
  GuardLog = maps:get(Id, ShiftsSummary, #{total => 0}),
  UpdatedLog = lists:foldl(fun add_interval/2, GuardLog, SleepIntervals),
  maps:put(Id, UpdatedLog, ShiftsSummary).

add_interval(SleepInterval, GuardLog) ->
  UpdatedMinutes = lists:foldl(fun update_minute/2, GuardLog, SleepInterval),
  add_total(SleepInterval, UpdatedMinutes).

update_minute(Minute, GuardLog) ->
  maps:update_with(Minute, fun inc/1, 1, GuardLog).

inc (X) -> X + 1.

add_total(Interval, GuardLog) ->
  Duration = length(Interval),
  maps:update_with(total, fun (X) -> X + Duration end, Duration, GuardLog).

get_most_sleepy(ShiftsSummary) ->
  MostSleepyGuards = lists:sort(fun most_sleepy/2, maps:to_list(ShiftsSummary)),
  [{Id, GuardLog} | _] = MostSleepyGuards,
  get_worst_minute(GuardLog) * Id.

most_sleepy({_IdA, Ma}, {_IdB, Mb}) ->
  #{total := A} = Ma,
  #{total := B} = Mb,
  A >= B.

get_worst_minute(GuardLog) ->
  Minutes = maps:to_list(maps:remove(total, GuardLog)),
  WorstMinutes = lists:sort(fun worst_minute/2, Minutes),
  [{WorstMinute, _} | _] = WorstMinutes,
  WorstMinute.

worst_minute({_, A}, {_, B}) ->
  A >= B.

solution_2() ->
  Lines = readlines('input.txt'),
  Ordered = lists:sort(Lines),
  Records = lists:map(fun parse_record/1, Ordered),
  ShiftLogs = partition_per_shift(Records),
  ShiftsSummary = lists:foldl(fun summarize_intervals/2, #{}, ShiftLogs),
  get_most_steady_sleeper(ShiftsSummary).

get_most_steady_sleeper(ShiftsSummary) ->
  OnlySleepers = lists:filter(fun does_sleep/1, maps:to_list(ShiftsSummary)),
  WithOnlyWorstMinute = lists:map(fun with_only_worst_minute/1, OnlySleepers),
  BestSteadySleepers = lists:sort(fun steady_sleeper_by_worst_minute/2, WithOnlyWorstMinute),
  [{Id, {SteadyMinute, _}} | _] = BestSteadySleepers,
  Id * SteadyMinute.

does_sleep({_, #{total := Total}}) ->
  Total > 0.

with_only_worst_minute({Id, GuardLog}) ->
  Minute = get_worst_minute(GuardLog),
  {Id, {Minute, maps:get(Minute, GuardLog)}}.

steady_sleeper_by_worst_minute({_, {_, CountA}}, {_, {_, CountB}}) ->
  CountA >= CountB.
