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
let inc a = a + 1

let viruspart1 = function
    | Some('#') -> turnRight, None, id
    | _ -> turnLeft, Some('#'), inc

let viruspart2 = function
    | Some('#') -> turnRight, Some('F'), id
    | Some('W') -> id, Some('#'), inc
    | Some('F') -> turnBack, None, id
    | _ -> turnLeft, Some('W'), id

let rec sporifica notclean iterations infect =
    let rec burst (pos, direction, notclean, count, remaining) =
        if remaining = 0 then
            count
        else
            let (directionOp, state, countOp) = infect (Map.tryFind pos notclean)
            let direction = directionOp direction
            match state with
            | Some(c) -> burst (step pos direction, direction, Map.add pos c notclean, countOp count, remaining - 1)
            | None ->  burst (step pos direction, direction, Map.remove pos notclean, countOp count, remaining - 1)

    burst ((0,0), Up, notclean, 0, iterations)

let iterations = 100
sporifica notclean iterations viruspart2

(* mutable = about 10 times faster
open System.Collections.Generic
let rec sporifica notclean iterations infect =
    let lookup = new Dictionary<_,_>()
    for(k,v) in notclean do lookup.Add(k,v)

    let rec burst (pos, direction, count, remaining) =
        if remaining = 0 then
            count
        else
            let (found, value) = lookup.TryGetValue(pos)
            let (directionOp, state, countOp) = infect (found, value)
            let direction = directionOp direction
            match state with
            | Some(c) ->
                if found then lookup.[pos] <- c else lookup.Add(pos, c)
                burst (step pos direction, direction, countOp count, remaining - 1)
            | None ->
                lookup.Remove(pos) |> ignore
                burst (step pos direction, direction, countOp count, remaining - 1)

    burst ((0,0), Up, 0, iterations)
*)
