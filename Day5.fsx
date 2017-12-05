let input = "0
3
0
1
-3
"
let array = input.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int

let jump index =
    let leap = array.[index]
    array.[index] <- leap + 1
    index + leap

let mutable index, jumps = 0, 0
while index >= 0 && index < array.Length do
    index <- jump index
    jumps <- jumps + 1

jumps
