// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module Program

open Util
open Square
open Move
open Piece
open Board


    

[<EntryPoint>]
let main argv = 
    
    let allSquares = Board.createDefaultSquares

    for s in allSquares do
        let res = logFun "Creating playing board %s" "test"
        let board = new PlayingBoard()
        let mutable piece = board.SetStartPosition s.Row s.Column
        board.DrawBoard()
        let mutable move = board.GetNextMove(piece)
        while (move.IsSome) do
            if System.Diagnostics.Debugger.IsAttached then
                System.Threading.Thread.CurrentThread.Join 1000 |> ignore
            
            piece <- board.PerformMove(move.Value)
            piece.LogPosition |> ignore

            move <- board.GetNextMove(piece)
            board.DrawBoard()

    0 // return an integer exit code
