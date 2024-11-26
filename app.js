const GRID = 5;
var game = function () {
  alert("You stepped on a pit, Game Over!");
  location.reload();
};

var home_event_win = function () {
  alert("You fulfilled the gold requirement, YOU WIN!");
  location.reload();
};

var home_event_lose = function () {
  alert(
    "You went back home without fulfilling the gold requirement, Game Over!"
  );
  location.reload();
};

// make child divs to all boxes and just assign robot class
let program = `
:- use_module(library(dom)).
:- use_module(library(lists)).
:- use_module(library(js)).
:- dynamic(player/2), dynamic(grid_size/1), dynamic(breeze/2), dynamic(glitter/2), dynamic(safe/2), dynamic(home/2).
:- dynamic(gold/1), dynamic(new_gold/1).  % Declare new_gold as dynamic
:- dynamic(has_visited_home/1).

% resets all dynamics
reset :-
	retractall(breeze(_, _)),
	retractall(glitter(_, _)),
	retractall(safe(_, _)),
	retractall(home(_, _)),
  retractall(player(_,_)),
  retractall(grid_size(_)).

% 5 by 5 grid
% 0 is clear
% 1 is glitter
% 2 is breeze
% 3 is pit
% 4 is glitter and breeze
% 6 is home
grid(	[0,1,2,0,1,
      1,2,3,2,0,
      2,3,2,2,0,
      0,2,2,3,2,
      6,0,0,4,0]).

      
move_away_from_home(X, Y) :-
    player(0, 4),  % Home position
    ((X \\== 0; Y \\== 4) -> assertz(has_visited_home(true)); true).


% Make the bg image of a cell in the grid to the specified css class
draw(X,Y, IMG) :-
  number_chars(X, A), % Convert number to a single atom in list
  number_chars(Y, B), 
  nth0(0,A,F),        % And get the atom from the list
  nth0(0,B,G),        % Done because there is no conversion predicate from number to atom
  atom_concat(F, G, R),
  get_by_id(R, D),
  add_class(D, IMG).


% draw the robot on the specified cell in the grid
draw_robot(X,Y) :-
  number_chars(X, A), % Convert number to a single atom in list
  number_chars(Y, B), 
  nth0(0,A,F),        % And get the atom from the list
  nth0(0,B,G),        % Done because there is no conversion predicate from number to atom
  atom_concat(F, G, R),
  atom_concat(R, '-child', P),
  get_by_id(P, D),
  add_class(D, 'robot').


undraw_robot(X,Y) :-
  number_chars(X, A),
  number_chars(Y, B),
  nth0(0,A,F),
  nth0(0,B,G),
  atom_concat(F, G, R),
  atom_concat(R, '-child', P),
  get_by_id(P, D),
  remove_class(D, 'robot').

undraw(X,Y,IMG) :-
  number_chars(X, A),
  number_chars(Y, B),
  nth0(0,A,F),
  nth0(0,B,G),
  atom_concat(F, G, R),
  get_by_id(R, D),
  remove_class(D, IMG).


% clears the css style in the keyboard representation
clear_controls :-
	findall(X, (get_by_class(control, X), remove_class(X, focus)), _).


  % show what button is being pressed
remark_control(Key) :-
	clear_controls,
	atom_concat('control-', Key, Id),
	get_by_id(Id, Control),
	add_class(Control, focus).

action(w) :- % up movement
  player(X1,Y1),
  undraw_robot(X1, Y1),
  ((Y1 - 1 < 0) -> Y2 = Y1; Y2 is Y1 - 1),
  retract(player(X1,Y1)),
  assertz(player(X1,Y2)),
  draw_robot(X1, Y2),
  land(X1,Y2),
   move_away_from_home(X1, Y2).

action(s) :- % down movement
  player(X1,Y1),
  undraw_robot(X1, Y1),
  grid_size(G),
  ((Y1 + 1 >= G) -> Y2 = Y1; Y2 is Y1 + 1),
  retract(player(X1,Y1)),
  assertz(player(X1,Y2)),
  draw_robot(X1, Y2),
  land(X1,Y2),
   move_away_from_home(X1, Y2).

action(a) :- % left movement
  player(X1,Y1),
  undraw_robot(X1, Y1),
  ((X1 - 1 < 0) -> X2 = X1; X2 is X1 - 1),
  retract(player(X1,Y1)),
  assertz(player(X2,Y1)),
  draw_robot(X2, Y1),
  land(X2,Y1),
   move_away_from_home(X2, Y1).

action(d) :- % right movement
  player(X1,Y1),
  undraw_robot(X1, Y1),
  grid_size(G),
  ((X1 + 1 >= G) -> X2 = X1; X2 is X1 + 1),
  retract(player(X1,Y1)),
  assertz(player(X2,Y1)),
  draw_robot(X2, Y1),
  land(X2,Y1),
   move_away_from_home(X2, Y1).

% note: query for getting the element E at index I (zero index)
% nth0(I, [a,b,c], E, _).

% X, Y are coordinates
% LIST is the grid to be passed
% OUT is the output
% D is the dimension (assumed to be square, usually 5)
check_grid(X,Y,LIST,OUT,D) :-
	I is X + D * Y,
	nth0(I,LIST,OUT, _).

  % draw glitter or breeze on the cell where they are queried to be true
show_entities(X,Y) :-
  glitter(X,Y) -> draw(X,Y,'glitter'),
  breeze(X,Y) -> draw(X, Y, 'breeze').


game_end(X) :-
  prop('game', G), apply(G, [0], H).

game_home_event(Gold) :-
    (   Gold > 2
    ->  prop('home_event_win', G), apply(G, [0], H)  % If gold > 2, trigger win event
    ;   prop('home_event_lose', G), apply(G, [0], H)  % If gold <= 2, trigger lose event
    ).

increment_gold_indicator(G) :-
  get_by_class('gold-amount', D),
  set_html(D, G).

  % checks the safety of the four directions, and checks if current cell is breeze, glitter or pit
land(X,Y) :- % land to X Y
	% check the grid
	grid(G),
  grid_size(GS),
  check_grid(X,Y,G,E,5),

   % Handle breeze tiles
    ((E == 2) -> (draw(X, Y, 'breeze'), assertz(breeze(X, Y))); true),

    % Handle glitter and glitter + breeze tiles
   

    % Handle pit tiles (Only trigger game end when stepping on pit tiles)
    ((E == 3) -> (draw(X, Y, 'pit'), game_end(P)); true),


  ((E == 1) ->
        (draw(X, Y, 'glitter'), assertz(glitter(X, Y)),
         retract(gold(CurrentGold)), % Retrieve current gold value
         NewGold is CurrentGold + 1, % Increment the gold count
         assertz(gold(NewGold))); % Update the gold count
    true),

    ((E == 4) ->
        (draw(X, Y, 'glitter-and-breeze'), assertz(breeze(X, Y)), assertz(glitter(X, Y)),
         retract(gold(CurrentGold)), % Retrieve current gold value
         NewGold is CurrentGold + 1, % Increment the gold count
         assertz(gold(NewGold))); % Update the gold count
    true),

    ((E == 6) -> 
        (draw(X, Y, 'home'),
        (has_visited_home(true) -> 
            gold(CurrentGold),  % Fetch the current gold
            game_home_event(CurrentGold)  % Pass the gold to the event
        ;   true))  % If not visited before, do nothing
    ;   true),  % If not the home tile, do nothing

    % Handle clear square (0) tiles
    ((E == 0) -> 
        (assertz(has_visited_home(true))); true),  % Mark home as visited

  (check_pit_all_direction(X,Y); true),
  is_safe(X, Y).

is_safe(X1,Y1) :- % space at X2, Y2 is safe if there is no breeze at X1, Y1
  % if not breeze then draw safe
  % I forgot to make the \ be the literal itself because this is a string pala
  % that was causing the bugs for the negation
	(\\+(breeze(X1, Y1)) -> draw_safe_left(X1,Y1), draw_safe_right(X1,Y1), draw_safe_down(X1,Y1), draw_safe_up(X1,Y1),
  draw_safe_current(X1, Y1); true).

  % I seperated the four directions to seperate predicate because there is a bug if all are in a single predicate
draw_safe_current(X1,Y1) :-
  draw(X1, Y1, 'safe'), assertz(safe(X1,Y1)).

draw_safe_left(X1,Y1) :-
  X2 is X1 - 1,
  (X2 >= 0) -> (draw(X2, Y1, 'safe'), assertz(safe(X2,Y1))); true.

draw_safe_right(X1,Y1) :-
  X2 is X1 + 1,
  grid_size(G),
  (X2 < G) -> (draw(X2, Y1, 'safe'), assertz(safe(X2,Y1))); true.

draw_safe_down(X1,Y1) :-
  Y2 is Y1 + 1,
  grid_size(G),
  (Y2 < G) -> (draw(X1, Y2, 'safe'), assertz(safe(X1,Y2))); true.

draw_safe_up(X1,Y1) :-
  Y2 is Y1 - 1,
  (Y2 >= 0) -> (draw(X1, Y2, 'safe'), assertz(safe(X1,Y2))); true.

check_pit_all_direction(X, Y) :-
  UP is Y-1, DOWN is Y+1, LEFT is X-1, RIGHT is X+1,
  (check_if_pit(X,UP);true),
  (check_if_pit(X,DOWN);true),
  (check_if_pit(RIGHT,Y);true),
  (check_if_pit(LEFT,Y)).


  % todo, show that a cell is a pit if all four sides are a breeze
check_if_pit(X,Y) :- % there may be a pit at X,Y if there is a breeze adjacent to it and not safe
  grid_size(G),
  UP is Y-1, DOWN is Y+1, LEFT is X-1, RIGHT is X+1,
  (breeze(X, DOWN), breeze(X, UP), breeze(LEFT, Y), breeze(RIGHT, Y)) -> draw(X,Y, 'pit').
  
init :-
  assertz(player(0,4)),
  assertz(breeze(-2,-2)), % placeholder so that getting term doesnt result in existence error
  assertz(grid_size(5)),
  assertz(home(0,4)),
	player(X,Y),
  draw(X,Y, 'home'),
  land(X,Y),
  draw_robot(X,Y),
	get_by_tag(body, Body),
	bind(Body, keyup, _, clear_controls),
	bind(Body, keydown, Event, (
		event_property(Event, key, Key),
    action(Key),
		remark_control(Key),
		prevent_default(Event)
	)).
`;
/*
    (Key=:=w -> Y2 is Y1-1; Y2 = Y1; true),
    (Key=:=d -> X2 is X1+1; X2 = X1; true),
    (Key=:=s -> Y2 is Y1+1; Y2 = Y1; true),
    (Key=:=a -> X2 is X1-1; X2 = X1; true),
*/
function makeGrid() {
  const map = document.querySelector(".map-container");
  map.style.gridTemplateColumns = `repeat(${GRID}, 1fr)`;
  map.style.gridTemplateRows = `repeat(${GRID}, 1fr)`;

  if (map.hasChildNodes()) {
    map.replaceChildren();
  }

  for (let y = 0; y < GRID; y++) {
    for (let x = 0; x < GRID; x++) {
      const pixel = document.createElement("div");
      const r = document.createElement("div");
      r.id = `${x}${y}-child`;
      pixel.classList.add("pixel");
      pixel.id = `${x}${y}`;
      map.appendChild(pixel);
      pixel.appendChild(r);
    }
  }
}

makeGrid();

function query() {
  let session = pl.create();
  session.consult(program);
  session.query(".");
  function inform(msg) {
    let outputElement = document.getElementById("output");
    outputElement.textContent += msg + "\n";
  }

  var callback = function (answer) {
    if (answer === false) {
      inform("DONE");
      return;
    }
    if (answer === null) {
      inform("TIMEOUT");
      return;
    }
    inform(pl.format_answer(answer));
    session.answer(callback);
  };
  // start the query loop
  session.answer(callback);
}

// Create session
var session = pl.create(1000);
// Consult program
session.consult(program, {
  success: function () {
    /* Program loaded correctly */
    console.log("consult succ");
  },
  error: function (err) {
    /* Error parsing program */
    console.log("consult fail" + err);
  },
});
// Query goal
session.query("init.", {
  success: function (goal) {
    /* Goal loaded correctly */
    console.log("succ query " + goal);
  },
  error: function (err) {
    /* Error parsing goal */
    console.log("err query " + err);
  },
});
// Find answers
session.answer({
  success: function (answer) {
    console.log(session.format_answer(answer)); // X = salad ;
    session.answer({
      success: function (answer) {
        console.log(session.format_answer(answer)); // X = apples ;
      },
      // ...
    });
  },
  fail: function () {
    /* No more answers */
  },
  error: function (err) {
    /* Uncaught exception */
    console.log("err" + err);
  },
  limit: function () {
    /* Limit exceeded */
  },
});
