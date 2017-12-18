let steps = 386
let findAt inserts find =
    let buffer = ResizeArray<int>()
    buffer.Add(0);
    let mutable iteration, pos = 0, 0
    while iteration < inserts do
        iteration <- iteration + 1
        pos <- 1 + (pos + steps) % iteration
        buffer.Insert(pos, iteration)
    let offset = buffer |> Seq.findIndex (fun x -> x = find)
    buffer |> Seq.item (offset + 1)

let part1 = findAt 2017 2017

let findAtOne inserts =
    let mutable iteration, pos, one = 0, 0, 0
    while iteration < inserts do
        iteration <- iteration + 1
        pos <- 1 + (pos + steps) % iteration
        if pos = 1 then one <- iteration
    one

let part2 = findAtOne 50000000
