//https://www.redblobgames.com/grids/hexagons/
let input = "se,sw,se,sw,sw"
let offset = function "n" -> (0,1,-1) | "ne" -> (1,0,-1) | "se" -> (1,-1,0) | "s" -> (0,-1,1) | "sw" -> (-1,0,1) | "nw" -> (-1,1,0) | _ -> (0,0,0)
let steps = input.Split ','
             |> Seq.map offset
             |> Seq.reduce (fun (a,b,c) (d,e,f) -> a+d,b+e,c+f)
             |> (fun (a,b,c) -> (abs a + abs b + abs c) / 2)
