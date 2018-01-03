let input = @"0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10"

type Component =
    { Id:int; A:int; B:int }
    member this.AnyPortIs value = this.A = value|| this.B = value
    member this.Score = this.A + this.B
    member this.OtherPort p =
        if this.A = p then
            this.B
        elif this.B = p then
            this.A
        else
            failwith "NO MATCH!?"

let components = input.Split([|'\r';'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
                 |> Array.map (fun c -> c.Split '/')
                 |> Array.mapi (fun i a -> { Id = i; A = int a.[0]; B = int a.[1] })

let rec build port list remaining =
    let matches = remaining |> Array.filter (fun (c:Component) -> c.AnyPortIs port)
                            |> Seq.collect (fun c -> build (c.OtherPort port) (c::list) (remaining |> Array.filter (fun x -> x.Id <> c.Id)) )
                            |> List.ofSeq
    list :: matches

let part1 = build 0 [] components |> List.map (fun l -> l |> List.sumBy (fun c -> c.Score)) |> List.max

let part2 = build 0 [] components |> Seq.map (fun l -> l |> List.length, l |> List.sumBy (fun c -> c.Score)) |> Seq.sortByDescending id |> Seq.head |> snd
    
