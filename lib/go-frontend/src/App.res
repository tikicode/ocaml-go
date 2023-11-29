%%raw(`import './App.css'`)

let makeGrid = (~rows, ~cols) => {
  let rowArray = Belt.List.makeBy(rows, i => i)
  let colArray = Belt.List.makeBy(cols, i => i)

  rowArray->Belt.List.map(row =>
    colArray->Belt.List.map(col =>
      <div key={string_of_int(row * cols + col)} className="intersection" />
    )
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
