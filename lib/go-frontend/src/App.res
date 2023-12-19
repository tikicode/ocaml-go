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

let updateGameBoard = (event, findOffset, row, col, playingAI) => {
  let mouseX = event->ReactEvent.Mouse.clientX
  let mouseY = event->ReactEvent.Mouse.clientY
  let squareSize = 26.203125
  let partialFunCoordinates = %raw(`
    function(event, mouseX, mouseY, squareSize, findOffset, row, col, playingAI) {
      let true_row = Math.abs(mouseY - event.target.getBoundingClientRect().top) < Math.abs(mouseY - event.target.getBoundingClientRect().bottom) ? row : row+1;
      let true_col = Math.abs(mouseX - event.target.getBoundingClientRect().left) < Math.abs(mouseX - event.target.getBoundingClientRect().right) ? col : col+1;
      var coordinates = true_row + " " + true_col;
      let moveEndpoint = "http://localhost:8080/move";
      let aiMoveEndpoint = "http://localhost:8080/move_ai";

      function createPiece(toPlay) {
        var target = event.target;
        var offset = findOffset(mouseX, mouseY, target.getBoundingClientRect().left, target.getBoundingClientRect().right, target.getBoundingClientRect().top, target.getBoundingClientRect().bottom, squareSize);
        var divAlreadyExists = document.querySelector('[data-coordinates="' + coordinates + '"]');
        var newDiv = document.createElement("div");
        // Apply styles to pieces
        newDiv.setAttribute("data-coordinates", coordinates);
        console.log(true_row + " " + true_col);
        newDiv.style.position = "absolute";
        newDiv.style.width = "20px";
        newDiv.style.height = "20px";
        newDiv.style.borderRadius = "50%";
        newDiv.style.overflow = "hidden";
        newDiv.style.left = target.getBoundingClientRect().left + offset[0] + "px";
        newDiv.style.top = target.getBoundingClientRect().top + offset[1] + "px";
        newDiv.style.background = toPlay;

        // Append the div to the document body
        document.body.appendChild(newDiv);
        if(playingAI) {
          makeMoveAI(aiMoveEndpoint);
        }
        }

      function createPieceAI(row, col) {
        let board_size = 500;
        const screenWidth = window.innerWidth;
        const screenHeight = window.innerHeight;
        let offset_row = ((screenWidth - board_size) / 2) + (col * squareSize) - 9;
        let offset_col = ((screenHeight - board_size) / 2) + (row * squareSize) + (2 * squareSize)- 9;
        var coordinates = row + " " + col;
        var divAlreadyExists = document.querySelector('[data-coordinates="' + coordinates + '"]');
        var newDiv = document.createElement("div");
        // Apply styles to pieces
        newDiv.setAttribute("data-coordinates", coordinates);
        console.log("AI COORDINATES: " + coordinates);
        newDiv.style.position = "absolute";
        newDiv.style.width = "20px";
        newDiv.style.height = "20px";
        newDiv.style.borderRadius = "50%";
        newDiv.style.overflow = "hidden";
        newDiv.style.left = offset_row + "px";
        newDiv.style.top = offset_col + "px";
        newDiv.style.background = "white";
        // Append the div to the document body
        document.body.appendChild(newDiv);
      }

      var alreadyExists = document.querySelector('[data-coordinates="' + coordinates + '"]');
      if(!alreadyExists) {
        let turnEndpoint = "http://localhost:8080/player_turn";
        getTurn(turnEndpoint)
        .then(_ => {
            makeMove(moveEndpoint, true_row, true_col);
          });
      }

      function removePiece(coordinates) {
        var divToRemove = document.querySelector('[data-coordinates="' + coordinates + '"]');
        if (divToRemove) {
          divToRemove.remove();
        }
      }

      async function makeMove(moveEndpoint, true_row, true_col) {
        console.log("Made Move");
        const response = await fetch(moveEndpoint, {
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
          removePiece(to_remove);
        }
      }

      async function makeMoveAI(aiEndpoint) {
        const response = await fetch(aiEndpoint, {
          method: "GET", 
          cache: "no-cache",
        });
        const move = await response.json();
        const movesArray = move.split(" ");
        const true_row = parseInt(movesArray[0], 10);
        const true_col = parseInt(movesArray[1], 10);
        makeMove(moveEndpoint, true_row, true_col);
        createPieceAI(true_row, true_col);
        console.log("AI-ROW: " + true_row);
        console.log("AI-COL: " + true_col);
      }

      async function getTurn(turnEndpoint) {
        const response = await fetch(turnEndpoint, {
          method: "GET", 
          cache: "no-cache",
        });
        const toPlay = await response.json();
        let playerTurn = (toPlay === "White") ? "white" : "black";
        createPiece(playerTurn);
      }
    }
  `)
  let boundingBox = partialFunCoordinates(
    event,
    mouseX,
    mouseY,
    squareSize,
    findOffset,
    row,
    col,
    playingAI,
  )
  Js.log(boundingBox)
}

let returnScore = %raw(` 
  function(scoreEndpoint) {
    getScore(scoreEndpoint);

    async function getScore(scoreEndpoint) {
      const response = await fetch(scoreEndpoint, {
        method: "GET", 
        cache: "no-cache",
      });
      const score = await response.json();

      // Remove all pieces on the board
      var allPieces = document.querySelectorAll('div[data-coordinates]');

      allPieces.forEach(function(div) {
        div.remove();
      });
      
      const whiteScore = score[0];
      const blackScore = score[1];
      
      // Remove grid lines from the board
      const goBoardElement = document.querySelector('.go-board');
      const intersections = goBoardElement.querySelectorAll('.intersection');
      intersections.forEach(intersection => {
        intersection.style.border = 'none';
      });
      
      // Display scores for white and black 
      const winnerDisplayElement = document.getElementById('winner-display');
      winnerDisplayElement.innerHTML = whiteScore > blackScore ? "White Wins!" : "Black Wins!";
      const scoreDisplayElement = document.getElementById('score-display');
      scoreDisplayElement.innerHTML = whiteScore + ' - ' + blackScore;
    }
  }
`)

let resetGame = %raw(`
    function(resetEndpoint) {
      reset_game(resetEndpoint);
      console.log("RESETTING");
      async function reset_game(resetEndpoint) {
        const response = await fetch(resetEndpoint, {
          method: "GET", 
          cache: "no-cache",
        });
        const reset = await response.json();
        var allPieces = document.querySelectorAll('div[data-coordinates]');

        allPieces.forEach(function(div) {
        div.remove();
      })
      }
    }
`)

let passTurn = %raw(`
    function(passTurnEndpoint) {
      pass_turn(passTurnEndpoint);
      console.log("PASSING TURN");
      async function pass_turn(passTurnEndpoint) {
        const response = await fetch(passTurnEndpoint, {
          method: "GET", 
          cache: "no-cache",
        });
        const turn = await response.json();
      }
    }
`)

let makeGrid = (~rows, ~cols) => {
  let rowArray = Belt.List.makeBy(rows, i => i)
  let colArray = Belt.List.makeBy(cols, i => i)
  let playingAI = ref(false)
  let resetGameEndpoint = "http://localhost:8080/reset_game"
  let scoreEndpoint = "http://localhost:8080/get_score"
  let passTurnEndpoint = "http://localhost:8080/pass_turn"
  let handleKeyDown = event => {
    switch ReactEvent.Keyboard.key(event) {
    | "E" => returnScore(scoreEndpoint)
    | "A" =>
      playingAI := true
      resetGame(resetGameEndpoint)
    | "T" =>
      playingAI := false
      resetGame(resetGameEndpoint)
    | "P" => passTurn(passTurnEndpoint)
    | _ => ()
    }
  }
  rowArray->Belt.List.map(row =>
    colArray->Belt.List.map(col => {
      let handleClick = event => {
        updateGameBoard(event, findOffset, row, col, playingAI.contents)
      }
      <div
        key={string_of_int(row * cols + col)}
        className="intersection"
        onClick={handleClick}
        onKeyDown={event => handleKeyDown(event)}
        tabIndex=0
      />
    })
  )
  |> Belt.List.flatten
  |> Belt.List.toArray
  |> React.array
}

%%raw(`
  let resetEndpoint = "http://localhost:8080/reset_game";

  window.addEventListener('unload', async function (event) {
      console.log("RESET!");
      try {
          await fetch(resetEndpoint, {method: 'GET',});
      } catch (error) {
          console.error("Error resetting the game:", error);
      }
  });
`)

@react.component
let make = () =>
  <div className="container">
    <div className="go-board"> {makeGrid(~rows=19, ~cols=19)} </div>
  </div>
