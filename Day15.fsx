let iA, iB = 722L, 354L
let fA, fB = 16807L, 48271L
let div = 2147483647L
let iterations = 40000000

let rec unfold f state = 
  seq {
    match f state with
    | Some x ->
      yield x
      yield! unfold f x
    | None -> ()
  }

let next prev factor = prev * factor % div
let develop i f = unfold (fun prev -> Some(next prev f)) i
let compare (a, b) = (a ^^^ b) &&& 65535L = 0L

Seq.zip (develop iA fA) (develop iB fB) |> Seq.map compare |> Seq.take iterations |> Seq.filter id |> Seq.length
