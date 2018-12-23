-module(main).
-export([solution_1/0, solution_2/0
, input/0, test_input/0
, solve_sum/1, solve_value/1
, build_node/1
]).

solution_1() ->
  Input = input(),
  solve_sum(Input).

solution_2() ->
  Input = input(),
  solve_value(Input).

input() ->
  [Line] = readlines('input.txt'),
  parse_line(Line).

test_input() ->
  [Line] = readlines('input2.txt'),
  parse_line(Line).

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
  lists:map(fun to_int/1, string:lexemes(Line, " \n")).

to_int(String) -> 
  {Int, _} = string:to_integer(String),
  Int.

solve_sum(Ints) ->
  {Root, []} = build_node(Ints),
  meta_sum(Root).

solve_value(Ints) ->
  {Root, []} = build_node(Ints),
  node_value(Root).

build_node([]) -> 
  {nil, []};
build_node([ChildNum, MetaNum | Remaining]) ->
  {Children, Rest} = build_children(Remaining, ChildNum),
  {Meta, Unused} = lists:split(MetaNum, Rest),
  {{Meta, Children}, Unused}.

build_children(Buffer, 0) -> 
  {[], Buffer};
build_children(Buffer, N) ->
  {Child, Rest} = build_node(Buffer),
  {Children, Unused} = build_children(Rest, N - 1),
  {[Child | Children], Unused}.

meta_sum({Meta, Children}) ->
  ChildrenSums = lists:map(fun meta_sum/1, Children),
  lists:sum(Meta) + lists:sum(ChildrenSums).

node_value({Meta, []}) ->
  lists:sum(Meta);
node_value({Meta, Children}) ->
  ChildrenValues = lists:map(fun node_value/1, Children),
  Values = lists:map(
    fun (N) -> 
      if
        N == 0 orelse N > length(ChildrenValues) -> 0;
        true -> lists:nth(N, ChildrenValues)
      end
    end, 
    Meta),
  lists:sum(Values).
