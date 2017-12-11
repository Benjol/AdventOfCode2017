let input = "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108"
let lengths = input.Split([|','|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int
let count = 256
let list = Array.init count id
let mutable skip, cursor = 0, 0
for length in lengths do
    let slice = Seq.init length id 
                    |> Seq.map (fun offset -> (cursor + offset) % count, (cursor + length - offset - 1) % count) 
                    |> Seq.map (fun (index_from, index_to) -> index_to, list.[index_from]) 
                    |> Seq.toList
    if length > 1 then slice |> List.iter (fun (i, v) -> list.[i] <- v)
    cursor <- cursor + length + skip
    skip <- skip + 1
list.[0] * list.[1]
