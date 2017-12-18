let steps = 386
let target = 2017
let buffer = ResizeArray<int>()
buffer.Add(0);
let mutable iteration, pos = 0, 0
while iteration < target do
    iteration <- iteration + 1
    pos <- 1 + (pos + steps) % iteration
    buffer.Insert(pos, iteration)

let offset = buffer |> Seq.findIndex (fun x -> x = target)
let part1 = buffer |> Seq.item (offset + 1)
