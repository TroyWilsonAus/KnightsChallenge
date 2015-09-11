module Board

open System

open Util
open Square
open Move
open Piece

type InvalidStateException() =
    inherit System.Exception()


let createDefaultSquares = 
   seq { for row in 1 .. 8 do
            for col in 1 .. 8 do
                let newSquare = new Square(row, col, SquareStatus.Available)
                yield newSquare
        }

type PlayingBoardState (squares: List<Square>) =
    let getAvailableSquares = fun _ -> squares |> List.filter( fun s -> s.Status = Available)

    member self.availableSquares = getAvailableSquares
    member self.Squares = squares

type PlayingBoard() =
    let logFunc = Util.logFun    
    let createInitialState = fun _ ->
                                let squares = createDefaultSquares |> Seq.toList
                                new PlayingBoardState(squares)

    let states = List.init 1 createInitialState

    member self.GetNextMove(piece: Piece) = 
        let moves = piece.BuildMoves
        let currentState = states |> List.last
        let squares = currentState.availableSquares
        //for m in moves do
        new Move(piece, piece)
    
    member self.SetStartPosition row column = 
        if (states |> List.length <> 1) then
            raise (InvalidStateException())
        let state = states |> List.head 
        let requiredSquare = state.Squares |> Seq.find (fun s -> s.Row = row && s.Column = column)
        let piece = new Piece(requiredSquare)
        //logFun "Setting start position: Row %d, Column %d" (requiredSquare.Row, requiredSquare.Column)
        piece
