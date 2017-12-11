let input = "{{<a!>},{<a!>},{<a!>},{<ab>}}"
type instruction = Normal | Ignore | Garbage
input |> Seq.fold (fun ((instruction, level, score) as input) c ->
                    match instruction with
                    | Normal -> match c with
                                    | '{' -> (Normal, level + 1, score)
                                    | '<' -> (Garbage, level, score)
                                    | '}' -> (Normal, level - 1, score + level)
                                    | _ -> input
                    | Ignore -> (Garbage, level, score)
                    | Garbage -> match c with
                                    | '!' -> (Ignore, level, score)
                                    | '>' -> (Normal, level, score)
                                    | _ -> input
                   ) (Normal, 0, 0) |> (fun (_,_,x) -> x)
