const GRID = 5;

let program = `
:- use_module(library(dom)).
:- use_module(library(lists)).
:- dynamic(player/2).

draw(X,Y, IMG) :-
  number_chars(X, A),
  number_chars(Y, B),
  nth0(0,A,F),
  nth0(0,B,G),
  atom_concat(F, G, R),
  get_by_id(R, D),
  add_class(D, IMG).

undraw(IMG) :-
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
  
init :-
  assertz(player(0,0)),
	player(X,Y),
  draw(X,Y,'robot'),
	get_by_tag(body, Body),
	bind(Body, keyup, _, clear_controls),
	bind(Body, keydown, Event, (
		event_property(Event, key, Key),
    player(X1,Y1),
    X2 is X1 + 1,
    Y2 is Y1 + 1,
    retract(player(X1,Y1)),
    assertz(player(X2,Y2)),
    draw(X1, Y1, 'robot'),
		remark_control(Key),
		action(Doge, Key),
		prevent_default(Event)
	)).
`;

function makeGrid() {
  const map = document.querySelector(".map-container");
  map.style.gridTemplateColumns = `repeat(${GRID}, 1fr)`;
  map.style.gridTemplateRows = `repeat(${GRID}, 1fr)`;

  if (map.hasChildNodes()) {
    map.replaceChildren();
  }

  for (let x = 0; x < GRID; x++) {
    for (let y = 0; y < GRID; y++) {
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
