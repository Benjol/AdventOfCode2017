let input = 312051
type Direction = Up | Down | Left | Right
type Corner = { Direction: Direction; X: int; Y: int; Count: int }
let develop c (steps, dir) =
    match dir with
    | Up -> { Direction = dir; X = c.X; Y = c.Y + steps; Count = c.Count + steps }
    | Down -> { Direction = dir; X = c.X; Y = c.Y - steps; Count = c.Count + steps }
    | Left -> { Direction = dir; X = c.X - steps; Y = c.Y; Count = c.Count + steps }
    | Right -> { Direction = dir; X = c.X + steps; Y = c.Y; Count = c.Count + steps }
let workbackto target c =
    let diff = c.Count - target
    match c.Direction with
    | Up -> (c.X, c.Y - diff)
    | Down -> (c.X, c.Y + diff)
    | Left -> (c.X + diff, c.Y)
    | Right -> (c.X - diff, c.Y)
let manhattan (x,y) = abs x + abs y
let odd i = i % 2 = 1
let output = Seq.initInfinite id
                |> Seq.skip 1 //not interested in 0!
                |> Seq.collect (fun i -> if odd i then [(i, Right);(i, Up)] else [(i, Left);(i, Down)])
                |> Seq.scan develop { Direction = Right; X = 0; Y = 0; Count = 1}
                |> Seq.find (fun c -> c.Count > input)
                |> workbackto input
                |> manhattan
