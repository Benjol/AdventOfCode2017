//PART1:
let input = @"..#
#..
..."

let grid = input.Split '\n' |> Array.map Array.ofSeq
let xoffset, yoffset = (Array.length grid.[0] / 2, Array.length grid / 2)

let infected = grid |> Seq.mapi (fun y row -> row |> Seq.mapi (fun x c -> x,y,c))
                    |> Seq.concat
                    |> Seq.filter (fun (_,_,c) -> c = '#')
                    |> Seq.map (fun (x,y,c) -> x - xoffset, y - yoffset)
                    |> Set.ofSeq

let Up, Down, Left, Right = (0,-1), (0,1), (-1,0), (1,0)
let turnLeft (x,y) = (y,-x)
let turnRight (x,y) = (-y,x)
let step (x,y) (dx,dy) = (x + dx, y + dy)

let iterations = 10000
let rec sporifica infected iterations =
    let rec burst (pos, direction, infected, count, remaining) =
        if remaining = 0 then
            count
        elif infected |> Set.contains pos then
            let direction = turnRight direction
            burst (step pos direction, direction, Set.remove pos infected, count, remaining - 1)
        else
            let direction = turnLeft direction
            burst (step pos direction, direction, Set.add pos infected, count + 1, remaining - 1)
    burst ((0,0), Up, infected, 0, iterations)

sporifica infected iterations

//PART2:
let input = @"..#
#..
..."

let grid = input.Split '\n' |> Array.map Array.ofSeq
let xoffset, yoffset = (Array.length grid.[0] / 2, Array.length grid / 2)

let notclean = grid |> Seq.mapi (fun y row -> row |> Seq.mapi (fun x c -> x,y,c))
                    |> Seq.concat
                    |> Seq.filter (fun (_,_,c) -> c = '#')
                    |> Seq.map (fun (x,y,c) -> (x - xoffset, y - yoffset), c)
                    |> Map

let Up, Down, Left, Right = (0,-1), (0,1), (-1,0), (1,0)
let turnLeft (x,y) = (y,-x)
let turnRight (x,y) = (-y,x)
let turnBack (x,y) = (-x,-y)
let step (x,y) (dx,dy) = (x + dx, y + dy)

let iterations = 10000000
let rec sporifica notclean iterations =
    let rec burst (pos, direction, notclean, count, remaining) =
        if remaining = 0 then
            count
        else
            match Map.tryFind pos notclean with
            | Some('#') ->
                let direction = turnRight direction
                burst (step pos direction, direction, Map.add pos 'F' notclean, count, remaining - 1)
            | Some('W') ->
                burst (step pos direction, direction, Map.add pos '#' notclean, count + 1, remaining - 1)
            | Some('F') ->
                let direction = turnBack direction
                burst (step pos direction, direction, Map.remove pos notclean, count, remaining - 1)
            | _ ->
                let direction = turnLeft direction
                burst (step pos direction, direction, Map.add pos 'W' notclean, count, remaining - 1)
    burst ((0,0), Up, notclean, 0, iterations)

sporifica notclean iterations
