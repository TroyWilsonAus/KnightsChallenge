// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module Program

open Util
open Square
open Move
open Piece
open Board


let LogFailure = fun(s : Square) ->
    logFun "Failed to use all squares starting at Row: %d, Col: %d" s.Row, s.Column

let LogSuccess = fun(s: Square, b :PlayingBoard) ->
    logFun "Used all squares starting at Row: %d, Col: %d" s.Row, s.Column
    // Need to log to file all moves

let CheckSquare = fun(s: Square) -> 
    let res = logFun "Creating playing board %s" "test"
    let board = new PlayingBoard()
    let mutable piece = board.SetStartPosition s.Row s.Column
    board.DrawBoard()
    let mutable move = board.GetNextMove(piece)
    while (move.IsSome) do                        
        piece <- board.PerformMove(move.Value)
        piece.LogPosition |> ignore
        board.DrawBoard()

        move <- board.GetNextMove(piece)
        
    match board.AllSquaresUsed() with
        | true  -> LogSuccess(s, board) |> ignore
        | false -> LogFailure(s)  |> ignore
            
[<EntryPoint>]
let main argv = 
    
    let allSquares = Board.createDefaultSquares
    for s in allSquares do
        CheckSquare(s)

    0 // return an integer exit code
