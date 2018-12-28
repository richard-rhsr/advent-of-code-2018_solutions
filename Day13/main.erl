-module(main).
-export([solution_1/0, solution_2/0
, input/0, test_input_1/0, test_input_2/0
, solve_collision/1, solve_last_one/1
, empty_rails/2, fill_rails/2
, print_char/1
]).

print_char(C) ->
  io:format("~p = '~s'~n", [C, [C]]).

solution_1() ->
  Input = input(),
  solve_collision(Input).

solution_2() ->
  Input = input(),
  solve_last_one(Input).

input() ->
  Lines = readlines('input.txt'),
  lists:map(fun parse_line/1, Lines).

test_input_1() ->
  Lines = readlines('input2.txt'),
  lists:map(fun parse_line/1, Lines).

test_input_2() ->
  Lines = readlines('input3.txt'),
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
  lists:flatten(string:lexemes(Line, "\n\r")).

-record(rails, {width, height, grid}).
-record(cart, {direction, turn=left}).
-record(collision, {x, y}).

next_turn(#cart{turn=left} = Cart) -> Cart#cart{turn=straight};
next_turn(#cart{turn=straight} = Cart) -> Cart#cart{turn=right};
next_turn(#cart{turn=right} = Cart) -> Cart#cart{turn=left}.

solve_collision(Lines) ->
  {Rails, Carts} = new_rails(Lines),
  print_rails(Rails),
  find_collision(Rails, Carts).

solve_last_one(Lines) ->
  {Rails, Carts} = new_rails(Lines),
  print_rails(Rails),
  continue_until_alone(Rails, Carts).

new_rails([First|_] = Lines) ->
  Heigth = length(Lines),
  Width = length(First),
  Content = lists:flatten(Lines),
  NoRails = empty_rails(Width, Heigth),
  {Rails, _Index, Carts} = lists:foldl(fun fill_rails/2, {NoRails, 0, #{}}, Content),
  {Rails, Carts}.

empty_rails(Width, Heigth) ->
  EmptyGrid = array:new(Heigth * Width, {default, $\ }),
  #rails{width=Width, height=Heigth, grid=EmptyGrid}.

fill_rails($\ , {#rails{}, Index, _Carts} = State) -> setelement(2, State, Index + 1);
fill_rails($| = Char, {#rails{}, _, _} = State) -> add_rail(Char, State);
fill_rails($- = Char, {#rails{}, _, _} = State) -> add_rail(Char, State);
fill_rails($\\ = Char, {#rails{}, _, _} = State) -> add_rail(Char, State);
fill_rails($/ = Char, {#rails{}, _, _} = State) -> add_rail(Char, State);
fill_rails($+ = Char, {#rails{}, _, _} = State) -> add_rail(Char, State);
fill_rails(Char, {#rails{}, _, _} = State) -> add_cart_n_rail(Char, State).
  
add_rail(Char, {#rails{grid=Array}=Rails, Index, Carts}) ->
  {Rails#rails{grid=array:set(Index, Char, Array)}, Index + 1, Carts}.

add_cart_n_rail($^, {#rails{}, _, _} = State) -> add_rail($|, add_cart(#cart{direction=up}, State));
add_cart_n_rail($v, {#rails{}, _, _} = State) -> add_rail($|, add_cart(#cart{direction=down}, State));
add_cart_n_rail($>, {#rails{}, _, _} = State) -> add_rail($-, add_cart(#cart{direction=right}, State));
add_cart_n_rail($<, {#rails{}, _, _} = State) -> add_rail($-, add_cart(#cart{direction=left}, State)).

add_cart(Cart, {#rails{} = Rails, Index, Carts}) ->
  NewCarts = maps:put(get_coords(Rails, Index), Cart, Carts),
  {Rails, Index, NewCarts}.

get_coords(#rails{width=W}, Index) ->
  Y = Index div W,
  X = Index rem W,
  {Y, X}.

get_index(#rails{width=W}, {Y, X}) ->
  Y * W + X.

get_rail(#rails{grid=Array} = Rails, {_, _} = Coord) ->
  array:get(get_index(Rails, Coord), Array).

find_collision(_, #collision{x=X, y=Y}) -> {X, Y};
find_collision(#rails{} = Rails, Carts) ->
  Coords = maps:keys(Carts),
  NewCarts = lists:foldl(
    fun (Coord, CurrentCarts) -> 
      Rail = get_rail(Rails, Coord),
      make_move(Coord, CurrentCarts, Rail)
    end, Carts, Coords),
  find_collision(Rails, NewCarts).

make_move(_, #collision{} = Col, _) -> Col;
make_move({_, _} = Coord, Carts, Rail) ->
  {Cart, WithoutCart} = maps:take(Coord, Carts),
  {{Y, X} = NextCoord, NextCart} = next_pos(Coord, Cart, Rail),
  case maps:is_key(NextCoord, WithoutCart) of
    true -> #collision{x=X, y=Y};
    false -> maps:put(NextCoord, NextCart, WithoutCart)
  end.

next_pos({_, _} = C, #cart{direction=up} = Cart, $|) -> move_up(C, Cart);
next_pos({_, _} = C, #cart{direction=down} = Cart, $|) -> move_down(C, Cart);
next_pos({_, _} = C, #cart{direction=left} = Cart, $-) -> move_left(C, Cart);
next_pos({_, _} = C, #cart{direction=right} = Cart, $-) -> move_right(C, Cart);
next_pos({_, _} = C, #cart{direction=up} = Cart, $\\) -> go_left(C, Cart);
next_pos({_, _} = C, #cart{direction=down} = Cart, $\\) -> go_right(C, Cart);
next_pos({_, _} = C, #cart{direction=left} = Cart, $\\) -> go_up(C, Cart);
next_pos({_, _} = C, #cart{direction=right} = Cart, $\\) -> go_down(C, Cart);
next_pos({_, _} = C, #cart{direction=up} = Cart, $/) -> go_right(C, Cart);
next_pos({_, _} = C, #cart{direction=down} = Cart, $/) -> go_left(C, Cart);
next_pos({_, _} = C, #cart{direction=left} = Cart, $/) -> go_down(C, Cart);
next_pos({_, _} = C, #cart{direction=right} = Cart, $/) -> go_up(C, Cart);
next_pos({_, _} = C, #cart{turn=straight} = Cart, $+) -> keep_ahead(C, Cart);
next_pos({_, _} = C, #cart{turn=left} = Cart, $+) -> turn_left(C, Cart);
next_pos({_, _} = C, #cart{turn=right} = Cart, $+) -> turn_right(C, Cart).

move_left({Y, X}, #cart{} = Cart) -> {{Y, X - 1}, Cart}.
move_right({Y, X}, #cart{} = Cart) -> {{Y, X + 1}, Cart}.
move_up({Y, X}, #cart{} = Cart) -> {{Y - 1, X}, Cart}.
move_down({Y, X}, #cart{} = Cart) -> {{Y + 1, X}, Cart}.

go_left({_, _} = C, #cart{} = Cart) -> move_left(C, Cart#cart{direction=left}).
go_right({_, _} = C, #cart{} = Cart) -> move_right(C, Cart#cart{direction=right}).
go_down({_, _} = C, #cart{} = Cart) -> move_down(C, Cart#cart{direction=down}).
go_up({_, _} = C, #cart{} = Cart) -> move_up(C, Cart#cart{direction=up}).

keep_ahead({_, _} = C, #cart{direction=up} = Cart) -> move_up(C, next_turn(Cart));
keep_ahead({_, _} = C, #cart{direction=down} = Cart) -> move_down(C, next_turn(Cart));
keep_ahead({_, _} = C, #cart{direction=left} = Cart) -> move_left(C, next_turn(Cart));
keep_ahead({_, _} = C, #cart{direction=right} = Cart) -> move_right(C, next_turn(Cart)).

turn_left({_, _} = C, #cart{direction=up} = Cart) -> go_left(C, next_turn(Cart));
turn_left({_, _} = C, #cart{direction=down} = Cart) -> go_right(C, next_turn(Cart));
turn_left({_, _} = C, #cart{direction=left} = Cart) -> go_down(C, next_turn(Cart));
turn_left({_, _} = C, #cart{direction=right} = Cart) -> go_up(C, next_turn(Cart)).

turn_right({_, _} = C, #cart{direction=up} = Cart) -> go_right(C, next_turn(Cart));
turn_right({_, _} = C, #cart{direction=down} = Cart) -> go_left(C, next_turn(Cart));
turn_right({_, _} = C, #cart{direction=left} = Cart) -> go_up(C, next_turn(Cart));
turn_right({_, _} = C, #cart{direction=right} = Cart) -> go_down(C, next_turn(Cart)).

continue_until_alone(#rails{}, Carts) when map_size(Carts) == 1 -> 
  [{Y, X}] = maps:keys(Carts),
  {X, Y};
continue_until_alone(#rails{} = Rails, Carts) ->
  Coords = maps:keys(Carts),
  NewCarts = lists:foldl(
    fun (Coord, CurrentCarts) -> 
      Rail = get_rail(Rails, Coord),
      make_move_safe(Coord, CurrentCarts, Rail)
    end, Carts, Coords),
  continue_until_alone(Rails, NewCarts).

make_move_safe({_, _} = Coord, Carts, Rail) ->
  case maps:take(Coord, Carts) of
    error -> Carts; % ignore coord error of previously removed cart
    {Cart, WithoutCart} ->
      {NextCoord, NextCart} = next_pos(Coord, Cart, Rail),
      case maps:is_key(NextCoord, WithoutCart) of
        true -> maps:remove(NextCoord, WithoutCart);
        false -> maps:put(NextCoord, NextCart, WithoutCart)
      end
  end.

print_rails(#rails{grid=Grid, width=W}) ->
  Lines = get_lines(array:to_list(Grid), W),
  lists:foreach(fun (L) -> io:format("~s~n", [L]) end, Lines).

get_lines(Content, W) ->
  {Line, Rest} = lists:split(W, Content),
  case Rest of
    [] -> [Line];
    _ -> [Line | get_lines(Rest, W)]
  end.
