let input = "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5"

open System
open System.Collections.Generic

let lookup = input.Replace("<->", "").Split([|'\n';'\r'|], StringSplitOptions.RemoveEmptyEntries)
                  |> Seq.map (fun def -> def.Split([|',';' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int)
                  |> Seq.map (fun a -> a.[0], a.[1..])
                  |> dict

let output = List<int>()
output.Add(0)

let rec visit (current : int) =
    if output.Contains(current) then
        for v in lookup.[current] do
            if not <| output.Contains(v) then 
                output.Add(v)
                visit v


visit 0
output.Count
