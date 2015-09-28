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
    let getAvailableSquares = 
        squares |> List.filter( fun s -> s.Status = Available)

    member self.availableSquares = getAvailableSquares
    member self.Squares = squares

type PlayingBoard() =
    let logFunc = Util.logFun    
    let createInitialState = fun _ ->
                                let squares = createDefaultSquares |> Seq.toList
                                new PlayingBoardState(squares)

    let mutable states = List.init 1 createInitialState
    let CurrentState = fun () -> states |> List.head
        

    let updateStatus = fun (square : Square) ->
        let currentState = CurrentState()
        let squaresToRemove = currentState.Squares |> List.filter (fun s -> s.Column = square.Column && s.Row = square.Row )
        let mostSquares = currentState.Squares |> List.except squaresToRemove
        let squaresToAdd = seq { yield square} |> Seq.toList 
        let allSquares = mostSquares |> List.append squaresToAdd 
        let newState = new PlayingBoardState(allSquares)
        let newStateList = seq { yield newState} |> Seq.toList
        states <- states |> List.append newStateList

    let findSquare  = fun (possible: PossibleSquare) ->
        let currentState = CurrentState()
        let squares = currentState.availableSquares 
        let foundSquare = squares |> List.tryFind(fun s -> possible.Column = s.Column && possible.Row = s.Row)
        foundSquare

    let performMove (square : Square) = 
        let newSquare = square.ChangeStatus(SquareStatus.Current)
        updateStatus newSquare
        new Piece(newSquare)


    member self.GetNextMove(piece: Piece) = 
        let possibleMoves = piece.BuildMoves |> Seq.toList
        
        
        //for m in moves do
        let destination = possibleMoves |> List.tryFind(fun m -> 
                match findSquare m with 
                | Some s -> true
                | None _ -> false)
        match destination with
            | Some m -> Option.Some m
            | None _ -> Option.None
    
    member self.SetStartPosition row column = 
        if (states |> List.length <> 1) then
            raise (InvalidStateException())
        let state = states |> List.head 
        let requiredSquare = state.Squares |> Seq.find (fun s -> s.Row = row && s.Column = column)
        performMove requiredSquare

    


    member self.PerformMove(move : PossibleSquare) =
        let square = findSquare move
        match square with
        | Some s -> performMove s
        | None _ -> raise (InvalidStateException())