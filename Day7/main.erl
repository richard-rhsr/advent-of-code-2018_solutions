-module(main).
-export([solution_1/0, solution_2/0
, input/0, test_input/0
, solve_mono/1, solve_multi/3
, map_repr/1
]).

-define(is_empty_gb_set(S), (is_tuple(S) andalso size(S) == 2 andalso element(1, S) == 0)).
% is_empty_gb_set({0, _}) -> true;
% is_empty_gb_set({S, _}) -> false.

solution_1() ->
  Input = input(),
  solve_mono(Input).

solution_2() ->
  Input = input(),
  WorkerNumber = 4,
  BaseTime = 60,
  solve_multi(Input, WorkerNumber, BaseTime).

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
  {ok, Regex} = re:compile("Step ([A-Z]) must be finished before step ([A-Z]) can begin."),
  {match, [_, Requirement, Dependency]} = re:run(Line, Regex, [{capture, all, list}]),
  {list_to_atom(Requirement), list_to_atom(Dependency)}.

solve_mono(Pairs) ->
  Tasks = all_tasks(Pairs),
  {DepToReqs, ReqToDeps} = lists:foldl(fun add_relation/2, {#{}, #{}}, Pairs),
  Init = init_state(DepToReqs, ReqToDeps, Tasks),
  Executed = execute_mono(Init),
  result_repr(Executed).

all_tasks(Pairs) ->
  Init = gb_sets:new(),
  lists:foldl(
    fun ({A, B}, GbSet) -> 
      gb_sets:add(A, gb_sets:add(B, GbSet)) 
    end, 
    Init, 
    Pairs).

add_relation({Req, Dep}, {DepMap, ReqMap}) ->
  {maps:update_with(Dep, fun (PrevSet) -> gb_sets:add(Req, PrevSet) end, gb_sets:from_list([Req]), DepMap),
  maps:update_with(Req, fun (PrevSet) -> gb_sets:add(Dep, PrevSet) end, gb_sets:from_list([Dep]), ReqMap)}.

init_state(DepToReqs, ReqToDeps, AllTasks) ->
  WithRequirements = gb_sets:from_list(maps:keys(DepToReqs)),
  ReadyTasks = gb_sets:difference(AllTasks, WithRequirements),
  ExecutedTasks = [],
  {ReadyTasks, ExecutedTasks, DepToReqs, ReqToDeps}.

execute_mono({ReadyTasks, ExecutedTasks, _, _}) when ?is_empty_gb_set(ReadyTasks) -> 
  lists:reverse(ExecutedTasks);
execute_mono({ReadyTasks, _, _, _} = TaskState) ->
  NextTask = gb_sets:smallest(ReadyTasks),
  WithoutThisTask = gb_sets:delete(NextTask, ReadyTasks),
  NewState = mark_as_done(NextTask, setelement(1, TaskState, WithoutThisTask)),
  execute_mono(NewState).

mark_as_done(DoneTask, {ReadyTasks, ExecutedTasks, DepToReqs, ReqToDeps}) ->
  {Dependents, NewReqToDeps} = case maps:take(DoneTask, ReqToDeps) of
    error -> {gb_sets:new(), #{}};
    Pair -> Pair
  end,
  UpdatedReqs = lists:map(
    fun (Dep) ->
      {Dep, gb_sets:delete_any(DoneTask, maps:get(Dep, DepToReqs))}
    end,
    gb_sets:to_list(Dependents)),
  {ReadyNow, StillNotReady} = lists:partition(
    fun ({_, Set}) -> 
      gb_sets:size(Set) == 0 
    end, 
    UpdatedReqs),
  ReadyList = lists:map(fun ({Task, _}) -> Task end, ReadyNow),
  NewRemaining = gb_sets:union([ReadyTasks, gb_sets:from_list(ReadyList)]),
  ExcludeReady = maps:without(ReadyList, DepToReqs),
  NewDepToReqs = maps:merge(ExcludeReady, maps:from_list(StillNotReady)),
  NewExecuted = [DoneTask | ExecutedTasks],
  {NewRemaining, NewExecuted , NewDepToReqs, NewReqToDeps}.

result_repr(Atoms) ->
  lists:flatten(lists:map(fun (A) -> atom_to_list(A) end, Atoms)).

solve_multi(Pairs, WorkerNumber, BaseTime) ->
  Tasks = all_tasks(Pairs),
  {DepToReqs, ReqToDeps} = lists:foldl(fun add_relation/2, {#{}, #{}}, Pairs),
  Init = init_state(DepToReqs, ReqToDeps, Tasks),
  CurrentWorkers = [],
  ElapsedTime = 0,
  {Executed, TotalTime} = execute_multi(Init, CurrentWorkers, ElapsedTime, WorkerNumber, BaseTime),
  {result_repr(Executed), TotalTime}.

execute_multi({ReadyTasks, ExecutedTasks, _, _}, CurrentWorkers, ElapsedTime, _, _) 
  when ?is_empty_gb_set(ReadyTasks)  andalso (length(CurrentWorkers) == 0) -> 
    {lists:reverse(ExecutedTasks), ElapsedTime};
execute_multi({ReadyTasks, _, _, _} = TaskState, CurrentWorkers, ElapsedTime, WorkerNumber, BaseTime)
  when not ?is_empty_gb_set(ReadyTasks) andalso (WorkerNumber > length(CurrentWorkers)) ->
    {Task, RemainingTasks} = gb_sets:take_smallest(ReadyTasks),
    Executing = worker_execute(Task, BaseTime),
    NewWorkers = [Executing | CurrentWorkers],
    NewState = setelement(1, TaskState, RemainingTasks),
    execute_multi(NewState, NewWorkers, ElapsedTime, WorkerNumber, BaseTime);
execute_multi({_, _, _, _} = TaskState, CurrentWorkers, ElapsedTime, WorkerNumber, BaseTime) ->
  case lists:keytake(0, 2, CurrentWorkers) of
    {value, {Task, _}, NewWorkers} ->
      NewState = mark_as_done(Task, TaskState),
      execute_multi(NewState, NewWorkers, ElapsedTime, WorkerNumber, BaseTime);
    false ->
      NextEventTime = lists:min(
        lists:map(
          fun ({_, RemainingTime}) -> 
            RemainingTime 
          end, 
          CurrentWorkers)),
      NewWorkers = lists:map(
        fun({Task, RemainingTime}) -> 
          {Task, RemainingTime - NextEventTime} 
        end, 
        CurrentWorkers),
      NewElapsedTime = ElapsedTime + NextEventTime,
      execute_multi(TaskState, NewWorkers, NewElapsedTime, WorkerNumber, BaseTime)
  end.

worker_execute(Task, BaseTime) ->
  {Task, task_time(Task, BaseTime)}.

task_time(Atom, BaseTime) ->
  [Char | _] = atom_to_list(Atom),
  BaseTime + (Char - $A + 1).

map_repr(Map) ->
  List = lists:map(
    fun ({K, Set}) -> 
      io_lib:format("~p => ~p", [K, gb_sets:to_list(Set)]) 
    end, 
    maps:to_list(Map)),
  lists:flatten(lists:join("; ", List)).
