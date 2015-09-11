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
        let piece = board.SetStartPosition s.Row s.Column
        let move = board.GetNextMove(piece)
        while (true) do
            ignore
        

    0 // return an integer exit code
