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

type PlayingBoardState (squares: List<Square>, piece: option<Piece>) =
    let getAvailableSquares = 
        squares |> List.filter( fun s -> s.Status = Available)

    member self.availableSquares = getAvailableSquares
    member self.Squares = squares
    member self.PlayingPiece = piece

type PlayingBoard() =
    let mutable totalMoveCount = 0
    let mutable totalRollbackCount = 0
    let logFunc = Util.logFun 
    let increaseRollbackCount = fun () -> totalRollbackCount   <- totalRollbackCount + 1
    let increaseMoveCount = fun () -> totalMoveCount <- totalMoveCount + 1                                
    let createInitialState = fun _ ->
                                let squares = createDefaultSquares |> Seq.toList
                                new PlayingBoardState(squares, None)

    let mutable states = List.init 1 createInitialState
    let CurrentState = fun () -> states |> List.head

    
    let drawBoardFunc = fun () ->
        Console.Clear()
        let currentSquare = CurrentState().Squares |> List.find (fun s -> s.Status = SquareStatus.Current)

        let formatRow = fun(row) ->
            let squares = CurrentState().Squares |> List.filter (fun s -> s.Row = row) |> List.sortBy (fun s -> s.Column)
            let formatSquare = fun (square : Square) ->
                match square.Status with
                    | SquareStatus.Available -> " "
                    | SquareStatus.Current -> "C"
                    | SquareStatus.Used -> "X"
            
            let rowText = sprintf "| %s | %s | %s | %s | %s | %s | %s | %s |" 
                            (formatSquare (squares.Item 0))
                            (formatSquare (squares.Item 1))
                            (formatSquare (squares.Item 2))
                            (formatSquare (squares.Item 3))
                            (formatSquare (squares.Item 4))
                            (formatSquare (squares.Item 5))
                            (formatSquare (squares.Item 6))
                            (formatSquare (squares.Item 7))
                
            rowText

        for row in 1 .. 8 do
            let rowText = formatRow(row)
            Console.WriteLine rowText

        printfn "Piece at row: %d column %d" currentSquare.Row currentSquare.Column |> ignore
        printfn "Current move count: %d" (states.Length - 1)
        printfn "Total move count: %d" totalMoveCount
        printfn "Total rollback count: %d" totalRollbackCount    
        delayFunc()

    let changeSquaresToUsed = fun (squares :List<Square> ) ->
        seq {
            for s in squares do
                yield s.ChangeStatus(SquareStatus.Used)
            }
        

    let updateStatus = fun (square : Square, piece: Piece) ->
        let currentState = CurrentState()
        let squaresToRemove = currentState.Squares |> List.filter (fun s -> (s.Column = square.Column && s.Row = square.Row) || s.Status = SquareStatus.Current )
        let currentSquares = squaresToRemove |> List.filter (fun s -> s.Status = SquareStatus.Current)
        let usedSquares = changeSquaresToUsed(currentSquares)
        let mostSquares = currentState.Squares |> List.except squaresToRemove
        let squaresToAdd = seq { 
                                    yield square
                                    for s in usedSquares do
                                        yield s
                                } |> Seq.toList 
        let allSquares = mostSquares |> List.append squaresToAdd 
        let newState = new PlayingBoardState(allSquares,  option.Some(piece))
        let newStateList = seq { yield newState} |> Seq.toList
        states <- states |> List.append newStateList

    
    
    let findSquare  = fun (possible: PossibleSquare) ->
        let currentState = CurrentState()
        let squares = currentState.availableSquares 
        let foundSquare = squares |> List.tryFind(fun s -> possible.Column = s.Column && possible.Row = s.Row)
        foundSquare

    let performMove (square : Square, indexOfPossibleMove) = 
        let newSquare = square.ChangeStatus(SquareStatus.Current)
        let piece = new Piece(newSquare, indexOfPossibleMove)
        updateStatus( newSquare, piece)
        piece

   
    let rollBackMove = fun () ->        
        let lastIndex = CurrentState().PlayingPiece.Value.IndexOfMoveUsed
        states <- states.Tail
        increaseRollbackCount()
        lastIndex

    let getSkipNumber = fun(piece: Piece, shouldSkip : int) ->
        shouldSkip

    let rec getNextMove_Impl = fun (piece: Piece, indexToStartAt : int) ->
        let skipNumber = getSkipNumber(piece, indexToStartAt)
        let possibleMovesAll = piece.BuildMoves |> Seq.toList
        let possibleMoves = possibleMovesAll |> List.skip skipNumber

        increaseMoveCount()
        
        let tryPrevMov = fun () ->
            let newIndex  = rollBackMove() + 1            
            drawBoardFunc()
            
            let currentState = CurrentState()
            let newPiece = currentState.PlayingPiece
            getNextMove_Impl(newPiece.Value, newIndex)

        let destination = possibleMoves |> List.tryFind(fun m -> 
                match findSquare m with 
                | Some s -> true
                | None _ -> false)

        match destination with
            | Some m -> Option.Some m
            | None _ -> tryPrevMov()

    member self.GetNextMove(piece: Piece) = 
        getNextMove_Impl(piece, 0)
                                
    
    
    member self.SetStartPosition row column = 
        if (states |> List.length <> 1) then
            raise (InvalidStateException())
        let state = states |> List.head 
        let requiredSquare = state.Squares |> Seq.find (fun s -> s.Row = row && s.Column = column)
        performMove (requiredSquare, -1)

    member self.DrawBoard  = drawBoardFunc


    member self.PerformMove(move : PossibleSquare) =
        let square = findSquare move
        match square with
        | Some s -> performMove (s, move.Index)
        | None _ -> raise (InvalidStateException())