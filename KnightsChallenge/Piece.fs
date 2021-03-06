﻿module Piece

open System
open Square
open Util

type moveOffset (row, column) =
    member self.Row = row
    member self.Column = column

let private moveMap = seq {
                        yield new moveOffset(-2, 1)
                        yield new moveOffset(1,2)
                        yield new moveOffset(-1,2)
                        yield new moveOffset(2,1)
                        yield new moveOffset(2,-1)
                        yield new moveOffset(1,-2)
                        yield new moveOffset(-1,-2)
                        yield new moveOffset(-2,-1)
                       }

type Piece (position: Square, indexOfPossibleMove: int) = 
    
    let currentSquare = position    
    let logNewPosition = fun _ ->
        //printfn "Piece at row: %d column %d" position.Row position.Column |> ignore
        //if System.Diagnostics.Debugger.IsAttached then
        //    System.Threading.Thread.CurrentThread.Join 100 |> ignore
        ignore
        
    
    
    let buildMoves = 
            seq{
                for o in moveMap do
                    let index = moveMap |> Seq.findIndex(fun s -> o.Row = s.Row && o.Column = s.Column)
                    yield new PossibleSquare(currentSquare.Row + o.Row, currentSquare.Column + o.Column, index)
                }
    

    member self.BuildMoves = buildMoves
    member self.LogPosition = logNewPosition
    member self.IndexOfMoveUsed = indexOfPossibleMove