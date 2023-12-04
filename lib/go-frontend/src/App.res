%%raw(`import './App.css'`)

let updateGameBoard = (event, tempWhiteToPlay) => {
  let mouseX = event->ReactEvent.Mouse.clientX
  Js.log(mouseX)
  let mouseY = event->ReactEvent.Mouse.clientY
  Js.log(mouseY)
  let partialFunCoordinates = %raw(`
    function(event, tempWhiteToPlay) {
      var target = event.target;
      if (mouseX >= target.getBoundingClientRect().left 
      && mouseX <= target.getBoundingClientRect().right
      && mouseY >= target.getBoundingClientRect().top 
      && mouseY <= target.getBoundingClientRect().bottom) {
          var newDiv = document.createElement("div");
          var offset = 16;
          // Apply styles
          newDiv.style.position = "absolute";
          newDiv.style.width = "20px";
          newDiv.style.height = "20px";
          newDiv.style.borderRadius = "50%";
          newDiv.style.overflow = "hidden";
          newDiv.style.left = target.getBoundingClientRect().left + offset + "px";
          newDiv.style.top = target.getBoundingClientRect().top + offset + "px";
          var whiteToPlay = tempWhiteToPlay.contents
          if (whiteToPlay) {
            newDiv.style.background = "white";
          } else {
            newDiv.style.background = "black";
          }
          // Append the div to the document body (or another container element)
          document.body.appendChild(newDiv);
      }
      return [target.getBoundingClientRect().left, target.getBoundingClientRect().right , target.getBoundingClientRect().bottom , target.getBoundingClientRect().top ];
    }
  `)
  let boundingBox = partialFunCoordinates(event, tempWhiteToPlay)
  Js.log(boundingBox)
  tempWhiteToPlay := !tempWhiteToPlay.contents
}

let makeGrid = (~rows, ~cols) => {
  let rowArray = Belt.List.makeBy(rows, i => i)
  let colArray = Belt.List.makeBy(cols, i => i)
  let tempWhiteToPlay = ref(true)
  rowArray->Belt.List.map(row =>
    colArray->Belt.List.map(col => {
      let handleClick = event => updateGameBoard(event, tempWhiteToPlay)
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
