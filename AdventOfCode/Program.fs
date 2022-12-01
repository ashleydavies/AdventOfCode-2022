module AdventOfCode.Program

open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

let solutions = [
    DayOne.solution
]

[<EntryPoint>]
let main argsArray =
    let (dayNumber, args) =
        match (argsArray |> Array.toList) with
        | dayNumString :: args -> (dayNumString |> int |> (-)1, args)
        | [] -> failwith "Please provide the day number as an argument"

    solutions[dayNumber] args
    0
