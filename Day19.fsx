let input = @"     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 
"

let grid = input.Split([|'\r';'\n'|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map Array.ofSeq

// .[y].[x]
let maxy = Array.length grid - 1
let maxx = Array.length grid.[0] - 1

type Direction = Up | Down | Left | Right
type Instruction = Continue | Turn | Collect of char | Stop

let parse char =
    match char with
    | '|' | '-' -> Continue
    | '+' -> Turn
    | ' ' -> Stop
    | _ -> Collect(char)

let nextxy (x, y) direction =
    match direction with
    | Up -> x, y - 1
    | Down -> x, y + 1
    | Left -> x - 1, y
    | Right -> x + 1, y

let rec Walk (x, y) direction collect steps =
    //printfn "%A %A %A %A" x y direction grid.[y].[x]
    match parse grid.[y].[x] with
    | Continue -> Walk (nextxy (x,y) direction) direction collect (steps + 1)
    | Collect(c) -> Walk (nextxy (x,y) direction) direction (c::collect) (steps + 1)
    | Turn ->  match direction with
               | Up | Down ->
                                if x = 0 then Walk (1, y) Right collect (steps + 1)
                                elif x = maxx then Walk (maxx - 1, y) Left collect (steps + 1)
                                elif grid.[y].[x + 1] = ' ' then Walk (x - 1, y) Left collect (steps + 1)
                                else Walk (x + 1, y) Right collect (steps + 1)
               | _ ->
                                if y = 0 then Walk (x, 1) Down collect (steps + 1)
                                elif y = maxy then Walk (x, maxy - 1) Up collect (steps + 1)
                                elif grid.[y + 1].[x] = ' ' then Walk (x, y - 1) Up collect (steps + 1)
                                else Walk (x, y + 1) Down collect (steps + 1)
    | Stop -> steps, List.rev collect |> Array.ofList |> System.String

let startx = Array.findIndex (fun c -> c = '|') grid.[0]

Walk (startx, 0) Down [] 0
