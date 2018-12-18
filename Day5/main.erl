-module(main).
-export([solution_1/0, solution_2/0]).

solution_1() ->
  [Line] = readlines('input.txt'),
  Inert = fully_react(Line),
  length(Inert).

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

% slow solution, probably n² because of slices with mid-string deletion
fully_react(Polymer) when erlang:is_list(Polymer) -> 
  fully_react({Polymer, 1});
fully_react({Polymer, Position}) when Position >= erlang:length(Polymer) -> 
  Polymer;
fully_react({Polymer, Position}) ->
  First = lists:nth(Position, Polymer),
  Second = lists:nth(Position + 1, Polymer),
  Case1 = string:to_lower(First) == Second andalso string:to_upper(Second) == First,
  Case2 = string:to_upper(First) == Second andalso string:to_lower(Second) == First,
  if
    Case1 orelse Case2 -> react({Polymer, Position});
    true -> fully_react({Polymer, Position + 1})
  end.

react({String, Position}) ->
  Index = Position - 1,
  Len = length(String),
  Remaining = string:concat(
    string:slice(String, 0, Index), 
    string:slice(String, Index + 2, Len)
  ),
  fully_react({Remaining, max(Position - 1, 1)}).

solution_2() ->
  [Line] = readlines('input.txt'),
  Types = sets:to_list(sets:from_list(lists:map(fun (C) -> string:to_lower(C) end, Line))),
  Improved = lists:map(fun (C) -> remove_type(Line, C) end, Types),
  ImprovedInert = lists:map(fun fully_react/1, Improved),
  FinalLengths = lists:map(fun length/1, ImprovedInert),
  lists:min(FinalLengths).
  % [length(Line) |lists:map(fun length/1, Improved)].

remove_type(Polymer, Char) ->
  Lower = string:to_lower(Char),
  Upper = string:to_upper(Char),
  Without = string:lexemes(Polymer, [Lower, Upper]),
  lists:flatten(Without).
