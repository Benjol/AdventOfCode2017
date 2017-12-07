let input = @"pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"
open System.Text.RegularExpressions
let pattern = "(?<Parent>.+) \(\d+\)( -> (?<Children>\w+(, \w+)*))?"
let regex = Regex(pattern)
let parents, children = regex.Matches(input)
                            |> Seq.cast<Match>
                            |> Seq.map (fun m -> m.Groups.["Parent"].Value, m.Groups.["Children"].Value)
                            |> Array.ofSeq
                            |> Array.unzip

let parents' = parents |> Array.sort
let children' = children |> Array.collect (fun s -> s.Split([|", "|], System.StringSplitOptions.RemoveEmptyEntries))
                         |> Array.sort

Seq.zip parents' children' |> Seq.find (fun (p,c) -> p <> c) |> fst
