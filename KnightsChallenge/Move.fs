module Move

type Move (oldSquare, newSquare) =
    let previous = oldSquare
    let next = newSquare

    member self.Previous = previous
    member self.Next = next