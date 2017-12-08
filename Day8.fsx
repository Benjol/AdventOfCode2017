type Instruction = { Target:string; ToAdd: int; Source:string; Condition: int -> int -> bool; Value: int}
let parse (instruction:string) =
    let arr = instruction.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
    {
        Target = arr.[0]
        ToAdd = match arr.[1] with | "inc" -> int arr.[2] | _ -> int arr.[2] * -1
        Source = arr.[4]
        Condition = match arr.[5] with ">" -> (>) | "<" -> (<) | ">=" -> (>=) | "<=" -> (<=) | "!=" -> (<>) | _ -> (=)
        Value = int arr.[6]
    }

let apply reg ins =
    match Map.tryFind ins.Target reg, Map.tryFind ins.Source reg with
    | Some(t), Some(s) -> if ins.Condition s ins.Value then Map.add ins.Target (t + ins.ToAdd) reg else reg
    | Some(t), None -> if ins.Condition 0 ins.Value then Map.add ins.Target (t + ins.ToAdd) reg else reg
    | None, Some(s) -> if ins.Condition s ins.Value then Map.add ins.Target ins.ToAdd reg else reg
    | _ -> if ins.Condition 0 ins.Value then Map.add ins.Target ins.ToAdd reg else reg

let input = @"b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"

let instructions = input.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)
let register = instructions |> Array.map parse |> Array.fold apply Map.empty
register |> Map.toList |> List.map snd |> List.max
