//quite a challenge, especially getting the transformations right
let input = "../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#"

let initial = ".#.
..#
###"

let flip (pattern:string) =
    match Array.ofSeq pattern with
    | [|a;b;c;d|] -> [|b;a;d;c|] |> System.String
    | [|a;b;c;d;e;f;g;h;i|] -> [|c;b;a;f;e;d;i;h;g|] |> System.String
    | _ -> failwith "boom!"

let rotate (pattern:string) =
    match Array.ofSeq pattern with
    | [|a;b;c;d|] -> [|b;d;a;c|] |> System.String
    | [|a;b;c;d;e;f;g;h;i|] -> [|c;f;i;b;e;h;a;d;g|] |> System.String
    | _ -> failwith "boom!"

let parseExpand (rule:string) =
    let arr = rule.Split([|" => "|], System.StringSplitOptions.RemoveEmptyEntries)
    let pattern, output = arr.[0].Replace("/",""), arr.[1].Replace("/","")
    let transforms = [id;flip;rotate;rotate >> flip;rotate >> rotate;rotate >> rotate >> flip; rotate >> rotate >> rotate;rotate >> rotate >> rotate >> flip]
    transforms |> List.map (fun t -> t pattern, (output, pattern))

let sqrt' = float >> sqrt >> int
    
let compose parts =
    let partcount = parts |> Array.length
    let partsize = parts |> Array.item 0 |> Array.length |> sqrt'
    let length = partcount * partsize * partsize
    let size = sqrt' length
    let size_in_parts = size / partsize

    let arr = Array.init length (fun _-> ' ')
    for i = 0 to length-1 do
        let x,y = i % size, i / size
        let partno = x / partsize + (y / partsize) * size_in_parts
        let partindex = x % partsize + (y % partsize) * partsize
        arr.[i] <- parts.[partno].[partindex]
    arr

let compose' parts = parts |> Array.map Array.ofSeq |> compose |> System.String

// [|"abcghimno"; "defjklpqr"; "stuyz0456"; "vwx123789"|] |> compose'
// [|"abgh"; "cdij"; "efkl"; "mnst"; "opuv"; "qrwx"; "yz45"; "0167"; "2389"|] |> compose'
let decompose partsize grid =
    let length = grid |> Array.length
    let size = sqrt' length
    let size_in_parts = size / partsize
    let partlength = partsize * partsize
    let partcount = length / partlength

    Array.init partcount (fun partno ->
            let part = Array.init partlength (fun _ -> ' ')
            for partindex = 0 to partlength - 1 do
                let px = partno % size_in_parts
                let py = partno / size_in_parts
                let x = px*partsize + partindex % partsize
                let y = py * partsize + partindex / partsize
                part.[partindex] <- grid.[y * size + x]
            part
        )

let decompose' partsize (grid:string) = grid |> Array.ofSeq |> decompose partsize |> Array.map System.String

//"abcdefghijklmnopqrstuvwxyz0123456789" |> decompose' 3;;
//"abcdefghijklmnopqrstuvwxyz0123456789" |> decompose' 2;;

let decomposeAuto (grid:string) =
    let size = grid.Length |> sqrt'
    if size % 2 = 0 then
        decompose' 2 grid
    else
        decompose' 3 grid

let lookup = input.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)
             |> Seq.collect parseExpand
             |> dict
   
let grid = initial.Replace("\n", "")
let iterations = 2
Seq.unfold (fun grid ->
        let grid' = grid |> decomposeAuto |> Array.Parallel.map (fun p -> lookup.[p]) |> compose'
        let t = grid' |> Seq.filter ((=) '#') |> Seq.length
        Some(t,grid')) grid |> Seq.item (iterations - 1)
