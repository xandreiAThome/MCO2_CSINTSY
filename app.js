const GRID = 4;
let program = `
            % Facts
            :- dynamic(parent/2)

            % Rules
            grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
        `;

function makeGrid() {
  const map = document.querySelector(".map-container");
  map.style.gridTemplateColumns = `repeat(${GRID}, 1fr)`;
  map.style.gridTemplateRows = `repeat(${GRID}, 1fr)`;

  if (map.hasChildNodes()) {
    map.replaceChildren();
  }

  for (let x = 0; x < GRID * GRID; x++) {
    const pixel = document.createElement("div");
    pixel.classList.add("pixel");
    pixel.classList.add("breeze");
    map.appendChild(pixel);
  }

  draw(document.querySelectorAll(".pixel"));
}

makeGrid();

function query() {
  let session = pl.create();
  session.consult(program);
  session.query("parent(X, Y).");
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
