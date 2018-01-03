let input = @"Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A."

type State = { Name:string; OnZero: Instruction; OnOne: Instruction }
and Instruction = { Write:int; Move:int; NextState: string}

open System.Text.RegularExpressions

let chunks = input.Split([|"\r\n\r\n";"\n\n"|], System.StringSplitOptions.RemoveEmptyEntries)
let steps = Regex.Match(chunks.[0], "\d+").Value |> int
let startState = Regex.Match(chunks.[0], "([A-Z])\.").Groups.Item(1).Value
let direction leftorright = if leftorright = "left" then -1 else 1

let states = chunks |> Seq.skip 1 |> Seq.map (fun chunk ->
                let matches = Regex.Matches(chunk, "(\w+)[\.:]") |> Seq.cast |> Seq.map (fun (m:Match) -> m.Groups.Item(1).Value) |> Seq.toArray
                {
                  Name = matches.[0]
                  OnZero = { Write = int matches.[2]; Move = direction matches.[3]; NextState = matches.[4] }
                  OnOne = { Write = int matches.[6]; Move = direction matches.[7]; NextState = matches.[8] }
                }
             ) |> Seq.map (fun s -> s.Name, s) |> Map


let rec turing tape pos step state =
    let state' = states.[state]
    if step = 0 then
        tape
    else
        let instruction = match Map.tryFind pos tape with
                          | Some(1) -> state'.OnOne
                          | _ -> state'.OnZero
        turing (Map.add pos instruction.Write tape) (pos + instruction.Move) (step - 1) instruction.NextState

let tape = Map [0,0]


let position = 0

let result = turing tape position steps startState
let part1 = result |> Map.toSeq|> Seq.sumBy snd
