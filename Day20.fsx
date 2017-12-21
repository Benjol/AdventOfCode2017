let input = @"p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"

type Particle = { P: float*float*float; V: float*float*float; A: float*float*float }

let parse (s:string) =
    match s.Split([|',';'<';'>'|], System.StringSplitOptions.RemoveEmptyEntries) with
    | [|_;px;py;pz;_;vx;vy;vz;_;ax;ay;az|] -> { P = (float px,float py,float pz); V = (float vx,float vy,float vz); A = (float ax,float ay,float az) }
    | _ -> failwith "boom!"
let particles =
    input.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parse

let manhattan (a,b,c) = (abs a + abs b + abs c)

//First variant: lowest acceleration, in case of a tie, sort by velocity
let part1 = particles |> Array.indexed |> Array.groupBy (fun (i,p) -> manhattan p.A) |> Array.sortBy fst |> Array.item 0 |> snd |> Array.minBy (fun (i,p) -> manhattan p.V) |> fst

//Alternative: find time at which all velocities have same signs as accelerations (so no 'turning round')
let timetostable {P = _; V =(vx,vy,vz); A = (ax,ay,az)} =
    let stable v a = v = 0. || (sign a) = (sign v)
    let tx = if stable vx ax then 0. else abs (vx - ax)
    let ty = if stable vy ay then 0. else abs (vy - ay)
    let tz = if stable vz az then 0. else abs (vz - az)
    max tx <| max ty tz

let stabletime = particles |> Array.Parallel.map timetostable |> Array.max

// This equation gives the future position of a point at time t
let futurepos {P = (px,py,pz); V = (vx,vy,vz); A = (ax,ay,az)} t =
    let fp p v a t = p + (v * t) + (a * t + a * t * t) / 2.
    fp px vx ax t, fp py vy ay t, fp pz vz az t

// find closest particle when stabletime is reached (gives right answer in mycase, but I'm not sure it always would)
particles |> Array.indexed |> Array.Parallel.map (fun (i,p) -> i, futurepos p stabletime) |> Array.minBy (fun (i,p) -> manhattan p) |> fst
// Check further out
particles |> Array.indexed |> Array.Parallel.map (fun (i,p) -> i, futurepos p 1000.) |> Array.minBy (fun (i,p) -> manhattan p) |> fst
particles |> Array.indexed |> Array.Parallel.map (fun (i,p) -> i, futurepos p 10000000.) |> Array.minBy (fun (i,p) -> manhattan p) |> fst
particles |> Array.indexed |> Array.Parallel.map (fun (i,p) -> i, futurepos p 100000000000. |> manhattan) |> Array.sortBy snd |> Array.take 4

//Just in case, do it the slow way
let tick p =
    let (px, py, pz), (vx, vy, vz), (ax, ay, az) = p.P, p.V, p.A
    { p with P = (px + vx + ax, py + vy + ay, pz + vz + az); V = (vx + ax, vy + ay, vz + az) }

Seq.unfold (fun p -> Some(p |> Array.indexed |> Array.minBy (fun (i,p) -> manhattan p.P) |> fst, p |> Array.map tick)) particles |> Seq.skip (int runtime) |> Seq.take 10 |> Seq.toList

//part 2
let prune particles = particles |> Seq.groupBy (fun p -> p.P) |> Seq.filter (fun (_,g) -> Seq.length g = 1) |> Seq.collect snd

//Hacky, just run it until 'stable' then suppose that it's the answer (worked for me!)
Seq.unfold (fun p -> Some(p |> Seq.length, p |> Array.Parallel.map tick |> prune |> Array.ofSeq)) particles |> Seq.take 300 |> Seq.iter (printfn "%A")
