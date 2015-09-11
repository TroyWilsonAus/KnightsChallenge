module Piece

open Square

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

type Piece (position: Square) = 
    let currentSquare = position
    let buildMoves = 
            seq{
                for o in moveMap do
                    yield new PossibleSquare(currentSquare.Row + o.Row, currentSquare.Column + o.Column)
                }

    member self.BuildMoves = buildMoves