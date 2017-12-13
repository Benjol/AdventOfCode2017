let input = "0: 3
1: 2
4: 4
6: 4"

let triangle p x = p - abs (x % (2 * p) - p)

input.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun def -> def.Split ':' |> Array.map int)
    |> Seq.map (fun a -> a.[0], a.[1])
    |> Seq.map (fun (depth, range) -> triangle (range - 1) depth, depth * range)
    |> Seq.filter (fun (pos,_) -> pos = 0)
    |> Seq.sumBy snd

//PART TWO:
let scans = 
    input.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun def -> def.Split ':' |> Array.map int)
        |> Array.map (fun a -> a.[0], a.[1])

let hits offset =
    scans
        |> Array.Parallel.map (fun (depth, range) -> triangle (range - 1) (depth + offset))
        |> Seq.filter (fun pos -> pos = 0)
        |> Seq.length

Seq.initInfinite id |> Seq.find (fun t -> hits t = 0)
