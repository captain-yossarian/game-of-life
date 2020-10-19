namespace Game



module Types =
    type Cell =
        | On
        | Off

    type Point = Point of (int * int)

    type Board = Map<Point, Cell>

    type GameState =
        { Board: Board
          Generation: int }

    type Direction =
        | N
        | E
        | S
        | W
        | NE
        | SE
        | SW
        | NW

module State =
    open Types

    val initialState: Board

module Utils =
    open Types

    val movePointBy: int * int -> Point -> Point
    val movePointTo: Direction -> int * int

    val movePoint: (Direction -> Point -> Point)
    val setInitialState: points:Point list -> board:Board -> Board
    val convertToNum: cell:Cell option -> int
