let input = @"set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"


type Instruction =
    | Snd of Value
    | Set of string * Value
    | Add of string * Value
    | Mul of string * Value
    | Mod of string * Value
    | Recover of Value
    | Jump of Value * Value
and Value = Register of string | Number of int64

let parse (str:string) =
    let arr = str.Split ' '
    let value str = match System.Int64.TryParse str with
                    | true, v -> Number(v)
                    | false, _ -> Register(str)
    match arr.[0] with
    | "snd" -> Snd(value arr.[1])
    | "set" -> Set(arr.[1], value arr.[2])
    | "add" -> Add(arr.[1], value arr.[2])
    | "mul" -> Mul(arr.[1], value arr.[2])
    | "mod" -> Mod(arr.[1], value arr.[2])
    | "rcv" -> Recover(value arr.[1])
    | "jgz" -> Jump(value arr.[1], value arr.[2])
    | _ -> failwith "boom!"

let instructions = input.Split([|'\r';'\n'|]) |> Array.map parse

let rec execute (ptr, frq, map) =
    let getReg r = match Map.tryFind r map with Some(n) -> n | None -> 0L
    let getVal = function Register(r) -> getReg r | Number(n) -> n
    if ptr < 0 || ptr >= instructions.Length then
        None
    else
//        printfn "-> %A" map
//        printf "%A %A %A" ptr instructions.[ptr] frq
        match instructions.[ptr] with
        | Snd(v) -> execute (ptr + 1, Some(getVal v), map)
        | Set(r, v) -> execute (ptr + 1, frq, Map.add r (getVal v) map)
        | Add(r, v) -> execute (ptr + 1, frq, Map.add r (getReg r + getVal v) map)
        | Mul(r, v) -> execute (ptr + 1, frq, Map.add r (getReg r * getVal v) map)
        | Mod(r, v) -> execute (ptr + 1, frq, Map.add r (getReg r % getVal v) map)
        | Recover(v) -> match getVal v with
                        | 0L -> execute(ptr + 1, frq, map)
                        | _ -> frq
        | Jump(v1,v2) -> match getVal v1 with
                            | 0L -> execute (ptr + 1, frq, map)
                            | _ -> execute (ptr + int (getVal v2), frq, map)

let part1 = execute (0, None, Map.empty)
