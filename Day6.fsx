let input = @"4	1	15	12	0	9	9	5	5	8	7	3	14	5	12	3"
let banks = input.Split([|'\t';' '|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int
let mostIndex banks = banks |> Array.mapi (fun i t -> i, t) |> Array.maxBy snd |> fst
let redistribute banks fromIndex = 
    let count = Array.length banks
    let blocks = banks.[fromIndex]
    banks.[fromIndex] <- 0
    for offset = 1 to blocks do
        let index = (offset + fromIndex) % count
        banks.[index] <- banks.[index] + 1

let loopCount banks = 
    let rec innerloop count banks (dic:ResizeArray<_>) key = 
        if dic.Contains(key) then
            count
        else
            dic.Add key
            redistribute banks (mostIndex banks)
            innerloop (count + 1) banks dic (sprintf "%A" banks)
    innerloop 0 banks (ResizeArray()) (sprintf "%A" banks)

loopCount banks
