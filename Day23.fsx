let input = @"set b 81
set c b
jnz a 2
jnz 1 5
mul b 100
sub b -100000
set c b
sub c -17000
set f 1
set d 2
set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13
jnz f 2
sub h -1
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -23"


type Instruction =
    | Set of string * Value
    | Sub of string * Value
    | Mul of string * Value
    | Jump of Value * Value
and Value = Register of string | Number of int64

let parse (str:string) =
    let arr = str.Split ' '
    let value str = match System.Int64.TryParse str with
                    | true, v -> Number(v)
                    | false, _ -> Register(str)
    match arr.[0] with
    | "set" -> Set(arr.[1], value arr.[2])
    | "sub" -> Sub(arr.[1], value arr.[2])
    | "mul" -> Mul(arr.[1], value arr.[2])
    | "jnz" -> Jump(value arr.[1], value arr.[2])
    | _ -> failwith "boom!"

let instructions = input.Split([|'\r';'\n'|]) |> Array.map parse

let rec execute (ptr, frq, map, counter) =
    let getReg r = match Map.tryFind r map with Some(n) -> n | None -> 0L
    let getVal = function Register(r) -> getReg r | Number(n) -> n
    if ptr < 0 || ptr >= instructions.Length then
        counter
    else
//        printfn "-> %A" map
//        printf "%A %A %A" ptr instructions.[ptr] frq
        match instructions.[ptr] with
        | Set(r, v) -> execute (ptr + 1, frq, Map.add r (getVal v) map, counter)
        | Sub(r, v) -> execute (ptr + 1, frq, Map.add r (getReg r - getVal v) map, counter)
        | Mul(r, v) -> execute (ptr + 1, frq, Map.add r (getReg r * getVal v) map, counter + 1)
        | Jump(v1,v2) -> match getVal v1 with
                            | 0L -> execute (ptr + 1, frq, map, counter)
                            | _ -> execute (ptr + int (getVal v2), frq, map, counter)

let part1 = execute (0, None, Map.empty, 0)
