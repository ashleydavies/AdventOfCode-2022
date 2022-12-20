module AdventOfCode.DayTen

open System.IO
open Microsoft.FSharp.Collections
open Helpers

type instruction = 
    | Noop
    | AddX of int

let expandInstruction = function
    | Noop -> [Noop]
    | AddX x -> [Noop; AddX x]

let parseInstruction = function
    | "noop" -> Noop
    | BeginsWith "addx" rest -> AddX (int rest)
    | other -> failwithf $"Invalid instruction %s{other}"

let execute instructions =
    let rec execute' acc = function
        | [] -> []
        | Noop :: rest -> acc :: execute' acc rest
        | AddX x :: rest -> acc :: execute' (acc + x) rest
    execute' 1 instructions

let render regValues =
    let rec render' idx = function
        | [] -> ()
        | spritePosition :: rest ->
            printf (if abs (spritePosition - idx) <= 1 then "#" else ".")
            if ((idx + 1) % 40 = 0) then printfn ""
            render' ((idx + 1) % 40) rest
    render' 0 regValues
 
let solution (args: string list) =
    let instructionLines = File.ReadAllText args.Head |> LineGrouping
    let instructions = instructionLines |> List.map parseInstruction |> List.map expandInstruction |> List.concat
    
    let regValues = execute instructions
    let signalStrength = regValues |> List.mapi (fun i x -> if (i+21) % 40 = 0 then x * (i+1) else 0) |> List.sum
    printfn $"%d{signalStrength}"
    render regValues
    ()
