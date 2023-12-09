%%raw(`import './App.css'`)

// module Response = {
//   type t<'data>
//   @send external json: t<'data> => Promise.t<'data> = "json"
// }

// type moves = {msg: string}

// let params = {
//   "method": "GET",
//   "mode": "no-cors",
// }

// type response<'data> = {
//   data: 'data,
//   code: int,
// }

// type res = response<moves>

// @val external fetch: (string, 'params) => Promise.t<Response.t<res>> = "fetch"

// let makeRequest = async url => {
//   open Promise
//   Js.log("HIT1")
//   fetch(url, params)->then(res => Response.json(res))->then(d => d.data)
//->catch(e -> Error("Back Error"))
//   switch data.code {
//   | 200 =>
//     Js.log("YES")
//     Ok(data.data)
//   | 500 => Error("Game not started")
//   | _ => Error("Internal Server Error")
//   }->resolve)
// ->catch(e => "Error"
// {
//   let msg = switch e {
//   | JsError(_) => "JS Error"
//   | _ => "Unexpected error occurred"
//   }
//   Error(msg)->resolve
// }
//)
//}

// let requestTest = makeRequest(apiUrl)

// Js.log(requestTest)
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

let updateGameBoard = (event, whiteToPlay, findOffset, row, col) => {
  let mouseX = event->ReactEvent.Mouse.clientX
  let mouseY = event->ReactEvent.Mouse.clientY
  let squareSize = 26.203125
  let partialFunCoordinates = %raw(`
    function(event, tempWhiteToPlay, mouseX, mouseY, squareSize, findOffset, row, col) {
      let true_row = Math.abs(mouseY - event.target.getBoundingClientRect().top) < Math.abs(mouseY - event.target.getBoundingClientRect().bottom) ? row : row+1;
      let true_col = Math.abs(mouseX - event.target.getBoundingClientRect().left) < Math.abs(mouseX - event.target.getBoundingClientRect().right) ? col : col+1;
      // console.log(true_row);
      // console.log(true_col);

      function createPiece() {
        var target = event.target;
        var offset = findOffset(mouseX, mouseY, target.getBoundingClientRect().left, target.getBoundingClientRect().right, target.getBoundingClientRect().top, target.getBoundingClientRect().bottom, squareSize);
        var newDiv = document.createElement("div");
        // Apply styles to pieces
        newDiv.setAttribute("data-coordinates", true_row + " " + true_col);
        console.log(true_row + " " + true_col);
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
          // removePiece([-10.0, -10.0]);
        }
        // Append the div to the document body
        document.body.appendChild(newDiv);

        return [target.getBoundingClientRect().left, target.getBoundingClientRect().right , target.getBoundingClientRect().bottom , target.getBoundingClientRect().top ];
      }
      createPiece();

      let move = "http://localhost:8080/move";
      makeMove(move);
      function removePiece(coordinates) {
        var divToRemove = document.querySelector('[data-coordinates="' + coordinates + '"]');
        if (divToRemove) {
          divToRemove.remove();
        }
      }
      async function makeMove(apiUrl) {
        console.log("Made Move");
        const response = await fetch(apiUrl, {
          method: "POST", 
          cache: "no-cache",
          body: (true_row+1) + " " + (true_col+1), 
        });
        const removePieces = await response.json();
        console.log(removePieces);

        for (let i = 0; i < removePieces.length; i++) {
          let pair = removePieces[i];

          let firstElem = pair[0];
          let secondElem = pair[1];
          let to_remove = firstElem + " " + secondElem;
          console.log(to_remove);
          removePiece(to_remove);
        }
      }
    }
  `)
  let boundingBox = partialFunCoordinates(
    event,
    whiteToPlay,
    mouseX,
    mouseY,
    squareSize,
    findOffset,
    row,
    col,
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
      let handleClick = event => {
        updateGameBoard(event, whiteToPlay, findOffset, row, col)
      }

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
