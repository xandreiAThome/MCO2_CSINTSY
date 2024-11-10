const GRID = 12;

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
