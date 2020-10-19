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


let draw () =
    let initial =
        setInitialState [ Point(2, 2); Point(2, 3) ] initialState


    for KeyValue (k, v) in initial do
        let (Point (row, column)) = k
        let point = Point(row, column)
        printfn "ROW: %A" table
        let cell = convertToNum (initial.TryFind(point))
        let tableRow = table.rows.[row]
        let td = document.createElement ("td")

        match cell with
        | 1 -> td.setAttribute (("active", "true"))
        | _ -> ()

        td.innerText <- " ."
        tableRow.appendChild (td) |> ignore
        printfn "ROW %A" table



// 'k' is the key, 'v' is the value



do printfn "Result %A" (draw ())

// Register our listener
myButton.onclick <-
    fun _ ->
        count <- count + 1
        myButton.innerText <- sprintf "You clicked: %i time(s 1)" count
