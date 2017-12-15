let iA, iB = 722L, 354L
let fA, fB = 16807L, 48271L
let div = 2147483647L
let iterations = 40000000

let rec unfold f state = Seq.unfold (fun state -> let r = f state in Some(r,r)) state

let next prev factor = prev * factor % div
let develop i f = unfold (fun prev -> next prev f) i
let compare (a, b) = (a ^^^ b) &&& 65535L = 0L

let part1 = Seq.zip (develop iA fA) (develop iB fB) |> Seq.map compare |> Seq.take iterations |> Seq.filter id |> Seq.length

let filterA, filterB = 4L, 8L
let iterations2 = 5000000
let part2 = Seq.zip 
                (develop iA fA |> Seq.filter (fun a -> a % filterA = 0L)) 
                (develop iB fB |> Seq.filter (fun b -> b % filterB = 0L)) 
                |> Seq.map compare |> Seq.take iterations2 |> Seq.sumBy (fun x -> if x then 1 else 0)
