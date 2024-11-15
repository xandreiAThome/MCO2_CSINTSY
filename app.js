const GRID = 5;

let program = `
:- use_module(library(dom)).
:- use_module(library(lists)).
:- dynamic(player/2), dynamic(grid_size/1), dynamic(breeze/2), dynamic(glitter/2), dynamic(safe/2), dynamic(home/2).

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
grid(	[0,1,2,0,1,
	1,2,3,2,0,
	2,3,2,2,0,
	0,2,2,3,2,
	0,0,0,4,0]).

draw(X,Y, IMG) :-
  number_chars(X, A), % Convert number to a single atom in list
  number_chars(Y, B), 
  nth0(0,A,F),        % And get the atom from the list
  nth0(0,B,G),        % Done because there is no conversion predicate from number to atom
  atom_concat(F, G, R),
  get_by_id(R, D),
  add_class(D, IMG).

undraw(X,Y,IMG) :-
  number_chars(X, A),
  number_chars(Y, B),
  nth0(0,A,F),
  nth0(0,B,G),
  atom_concat(F, G, R),
  get_by_id(R, D),
  remove_class(D, IMG).

clear_controls :-
	findall(X, (get_by_class(control, X), remove_class(X, focus)), _).

remark_control(Key) :-
	clear_controls,
	atom_concat('control-', Key, Id),
	get_by_id(Id, Control),
	add_class(Control, focus).

action(w) :- % up movement
  player(X1,Y1),
  undraw(X1, Y1, 'robot'),
  ((Y1 - 1 < 0) -> Y2 = Y1; Y2 is Y1 - 1),
  retract(player(X1,Y1)),
  assertz(player(X1,Y2)),
  draw(X1, Y2, 'robot').

action(s) :- % down movement
  player(X1,Y1),
  undraw(X1, Y1, 'robot'),
  grid_size(G),
  ((Y1 + 1 >= G) -> Y2 = Y1; Y2 is Y1 + 1),
  retract(player(X1,Y1)),
  assertz(player(X1,Y2)),
  draw(X1, Y2, 'robot').

action(a) :- % left movement
  player(X1,Y1),
  undraw(X1, Y1, 'robot'),
  ((X1 - 1 < 0) -> X2 = X1; X2 is X1 - 1),
  retract(player(X1,Y1)),
  assertz(player(X2,Y1)),
  draw(X2, Y1, 'robot').

action(d) :- % up movement
  player(X1,Y1),
  undraw(X1, Y1, 'robot'),
  grid_size(G),
  ((X1 + 1 >= G) -> X2 = X1; X2 is X1 + 1),
  retract(player(X1,Y1)),
  assertz(player(X2,Y1)),
  draw(X2, Y1, 'robot').

% note: query for getting the element E at index I (zero index)
% nth0(I, [a,b,c], E, _).

% X, Y are coordinates
% LIST is the grid to be passed
% OUT is the output
% D is the dimension (assumed to be square, usually 5)
check_grid(X,Y,LIST,OUT,D) :-
	I is X + D * Y,
	nth0(I,LIST,OUT, _).

show_entities(X,Y) :-
  glitter(X,Y) -> draw(X,Y,'glitter'),
  breeze(X,Y) -> draw(X, Y, 'breeze').

land(X,Y) :- % land to X Y
	% check the grid
	grid(G),
  check_grid(X,Y,G,E,5),
  ((E == 1) -> (draw(X,Y,'glitter'), assertz(glitter(X,Y))); true).

is_safe(X1,Y1,X2,Y2) :- % space at X2, Y2 is safe if there is no breeze at X1, Y1
	((\+breeze(X1,Y1)) -> assertz(safe(X2,Y2)), assertz(safe(X1,Y1)); true).

maybe_pit(X,Y) :- % there may be a pit at X,Y if there is a breeze adjacent to it and not safe
	A is X - 1,
	breeze(A,Y);
	A is X + 1,
	breeze(A,Y);
	A is Y - 1,
	breeze(X,A);
	A is Y + 1,
	breeze(X,A).

found_gold(X,Y) :- % gold is found at X,Y if there is glitter visible on it
	glitter(X,Y).
  
init :-
  assertz(player(0,4)),
  assertz(grid_size(5)),
	player(X,Y),
  draw(X,Y,'robot'),
	get_by_tag(body, Body),
	bind(Body, keyup, _, clear_controls),
	bind(Body, keydown, Event, (
		event_property(Event, key, Key),
    player(X1,Y1),
    land(X1,Y1),
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
      pixel.classList.add("pixel");
      pixel.id = `${x}${y}`;
      map.appendChild(pixel);
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
