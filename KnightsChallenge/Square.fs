﻿module Square

type SquareStatus = Available | Used | Current

type Square (row: int, column: int, status: SquareStatus) =
    member self.Row = row
    member self.Column = column
    member self.Status = status

[<CustomEquality>]
type PossibleSquare (row: int, column: int) =
    member self.Row = row
    member self.Column = column

   // override this.