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

let rec Walk (x, y) direction collect =
    //printfn "%A %A %A %A" x y direction grid.[y].[x]
    match parse grid.[y].[x] with
    | Continue -> Walk (nextxy (x,y) direction) direction collect
    | Collect(c) -> Walk (nextxy (x,y) direction) direction (c::collect)
    | Turn ->  match direction with
               | Up | Down ->
                                if x = 0 then Walk (1, y) Right collect
                                elif x = maxx then Walk (maxx - 1, y) Left collect
                                elif grid.[y].[x + 1] = ' ' then Walk (x - 1, y) Left collect
                                else Walk (x + 1, y) Right collect
               | _ ->
                                if y = 0 then Walk (x, 1) Down collect
                                elif y = maxy then Walk (x, maxy - 1) Up collect
                                elif grid.[y + 1].[x] = ' ' then Walk (x, y - 1) Up collect
                                else Walk (x, y + 1) Down collect
    | Stop -> List.rev collect

let startx = Array.findIndex (fun c -> c = '|') grid.[0]

Walk (startx, 0) Down []
