module App

open Browser.Dom
open Game.Utils
open Game.State
open Game.Types
// Mutable variable to count the number of times we clicked the button
let mutable count = 0

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton =
    document.querySelector (".my-button") :?> Browser.Types.HTMLButtonElement


let table =
    document.getElementById ("table") :?> Browser.Types.HTMLTableElement

let mutable board =
    setInitialState
        [ Point(6, 6)
          Point(7, 6)
          Point(8, 6)
          Point(6, 7)
          Point(7, 8) ]
        initialState

let draw (gameBoard: Board) =
    for KeyValue (k, v) in gameBoard do
        let (Point (row, column)) = k
        let point = Point(row, column)
        let cell = convertToNum (gameBoard.TryFind(point))
        printfn "ROW %A" row
        let tableRow = table.rows.[row]
        let td = document.createElement ("td")
        td.id <- string (row, column)

        match cell with
        | 1 -> td.setAttribute (("active", "true"))
        | _ -> ()

        td.innerText <- " ."
        tableRow.appendChild (td) |> ignore

let updateBoard (gameBoard: Board) =
    for KeyValue (k, v) in gameBoard do
        let (Point (row, column)) = k
        let point = Point(row, column)
        let cell = convertToNum (gameBoard.TryFind(point))

        let td =
            document.getElementById (string (row, column))

        match cell with
        | 1 -> td.setAttribute (("active", "true"))
        | _ -> td.removeAttribute ("active")

        td.innerText <- " ."

do draw board
// Register our listener
myButton.onclick <-
    fun _ ->
        board <- applyGeneration board
        updateBoard board

        myButton.innerText <- sprintf "You clicked: %i time(s 1)" count
