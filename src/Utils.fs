namespace Game

module Types =
    type Cell =
        | On
        | Off

    type Point = Point of (int * int)
    type Board = Map<Point, Cell>
    type PointBoard = Point * Board

    type GameState = { Board: Board; Generation: int }

    type Direction =
        | N
        | E
        | S
        | W
        | NE
        | SE
        | SW
        | NW

module Constants =
    open Types

    let ways = [ N; E; S; W; NE; SE; SW; NW ]

module State =
    open Types

    let cellCount = 99

    let cells = [ 0 .. cellCount ]

    let toCoordinates (index: int) =
        let row = index / 10
        let column = index % 10
        Point(row, column)

    let predicate (board: Board) cell = board.Add(toCoordinates cell, Off)

    let initialState = cells |> List.fold predicate Map.empty

module Utils =
    open Types
    open Constants

    let convertToNum cell =
        match cell with
        | Some On -> 1
        | _ -> 0


    let movePointBy ((rowShift, columnShift): int * int) (Point (row, column): Point) =
        Point(row + rowShift, column + columnShift)

    let movePointTo direction =
        match direction with
        | N -> (-1, 0)
        | E -> (0, 1)
        | S -> (1, 0)
        | W -> (0, -1)
        | NE -> (-1, 1)
        | SE -> (1, 1)
        | SW -> (1, -1)
        | NW -> (-1, -1)

    let cellValue (point, board: Board) = point |> board.TryFind |> convertToNum

    let neighborsCount pointBoard =
        let count acc _ = acc + cellValue pointBoard
        ways |> List.fold count 0

    let nextMove neighbors =
        match neighbors with
        | _ when neighbors < 2 -> Off
        | _ when neighbors >= 2 && neighbors <= 3 -> On
        | _ when neighbors > 3 -> Off
        | _ -> Off

    let setInitialState (points: Point list) (board: Board) =
        let predicate (board: Board) point = board.Add(point, On)
        points |> List.fold predicate board

    let computeGeneration = neighborsCount >> nextMove

    let movePoint = movePointTo >> movePointBy
