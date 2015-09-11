module Square

type SquareStatus = Available | Used | Current

type Square (row: int, column: int, status: SquareStatus) =
    let myRow = row
    let myColumn = column
    let myStatus = status
    member self.Row = myRow
    member self.Column = myColumn
    member self.Status = myStatus

type PossibleSquare (row, column) =
    member self.Row = row
    member self.Column = column