%%raw(`import './App.css'`)

let findOffset = (x, y, left, right, top, bottom, squareSize) => {
  if (
    x >= left - 1 &&
    x <= right + 1 - squareSize / 2 &&
    y >= top - 1 &&
    y <= bottom + 1 - squareSize / 2
  ) {
    [-10.0, -10.0]
  } else if (
    x >= left - 1 &&
    x <= right + 1 - squareSize / 2 &&
    y >= top - 1 + squareSize / 2 &&
    y <= bottom + 1
  ) {
    [-10.0, 15.5]
  } else if (
    x >= left - 1 + squareSize / 2 &&
    x <= right + 1 &&
    y >= top - 1 &&
    y <= bottom + 1 - squareSize / 2
  ) {
    [15.5, -10.0]
  } else if (
    x >= left - 1 + squareSize / 2 &&
    x <= right + 1 &&
    y >= top - 1 + squareSize / 2 &&
    y <= bottom + 1
  ) {
    [15.5, 15.5]
  } else {
    [0.0]
  }
}

let updateGameBoard = (event, whiteToPlay, findOffset) => {
  let mouseX = event->ReactEvent.Mouse.clientX
  let mouseY = event->ReactEvent.Mouse.clientY
  let squareSize = 26.203125
  let partialFunCoordinates = %raw(`
    function(event, tempWhiteToPlay, mouseX, mouseY, squareSize, findOffset) {
      var target = event.target;
      var offset = findOffset(mouseX, mouseY, target.getBoundingClientRect().left, target.getBoundingClientRect().right, target.getBoundingClientRect().top, target.getBoundingClientRect().bottom, squareSize);
      var newDiv = document.createElement("div");
      // Apply styles to pieces
      newDiv.style.position = "absolute";
      newDiv.style.width = "20px";
      newDiv.style.height = "20px";
      newDiv.style.borderRadius = "50%";
      newDiv.style.overflow = "hidden";
      newDiv.style.left = target.getBoundingClientRect().left + offset[0] + "px";
      newDiv.style.top = target.getBoundingClientRect().top + offset[1] + "px";
      var whiteToPlay = tempWhiteToPlay.contents
      if (whiteToPlay) {
        newDiv.style.background = "white";
      } else {
        newDiv.style.background = "black";
      }
      // Append the div to the document body
      document.body.appendChild(newDiv);

      return [target.getBoundingClientRect().left, target.getBoundingClientRect().right , target.getBoundingClientRect().bottom , target.getBoundingClientRect().top ];
    }
  `)
  let boundingBox = partialFunCoordinates(
    event,
    whiteToPlay,
    mouseX,
    mouseY,
    squareSize,
    findOffset,
  )
  Js.log(boundingBox)
  whiteToPlay := !whiteToPlay.contents
}

let makeGrid = (~rows, ~cols) => {
  let rowArray = Belt.List.makeBy(rows, i => i)
  let colArray = Belt.List.makeBy(cols, i => i)
  let whiteToPlay = ref(true)
  rowArray->Belt.List.map(row =>
    colArray->Belt.List.map(col => {
      let handleClick = event => updateGameBoard(event, whiteToPlay, findOffset)
      <div key={string_of_int(row * cols + col)} className="intersection" onClick={handleClick} />
    })
  )
  |> Belt.List.flatten
  |> Belt.List.toArray
  |> React.array
}

@react.component
let make = () =>
  <div className="container">
    <div className="go-board"> {makeGrid(~rows=19, ~cols=19)} </div>
  </div>
