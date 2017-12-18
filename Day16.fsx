let input = "s1,x3/4,pe/b"
let initial = "abcde"

type Step = Spin of int | Exchange of (int * int) | Partner of (char * char)
let parse (step:string) =
    let tup (arr:array<_>) = arr.[0], arr.[1]
    match step.[0] with
    | 's' -> Spin(step.Substring(1) |> int)
    | 'x' -> Exchange(step.Substring(1).Split '/' |> Array.map int |> tup)
    | _ -> Partner(step.Substring(1).Split '/' |> Array.map char |> tup)

let swap (arr:array<_>) a b =
    let tmp = arr.[a]
    arr.[a] <- arr.[b]
    arr.[b] <- tmp
    arr

let danse floor step =
    let length = Array.length floor
    match step with
    | Spin(count) -> Array.concat([|floor.[length - count..]; floor.[0..(length-count-1)]|])
    | Exchange(a,b) -> swap floor a b
    | Partner(x,y) ->
        let a = floor |> Array.findIndex (fun c -> c = x)
        let b = floor |> Array.findIndex (fun c -> c = y)
        swap floor a b

let apply instructions floor = instructions |> Seq.fold danse floor

let instructions = input.Split ',' |> Array.map parse
let floor = initial |> Seq.toArray

let part1 = floor.[*] |> apply instructions |> System.String

let iterations = 1000000000

let rec unfold f state = Seq.unfold (fun state -> let r = f state in Some(r,r)) state

let cycle = floor.[*] |> unfold (apply instructions) |> Seq.map System.String |> Seq.takeWhile ((<>) initial) |> Seq.toList
let cycle' = initial :: cycle
let offset = iterations % (List.length cycle')
let part2 = cycle'.[offset]
