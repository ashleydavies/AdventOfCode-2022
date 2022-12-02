module AdventOfCode.Program

open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open FSharpx

let solutions = [ DayOne.solution; DayTwo.solution ]

[<EntryPoint>]
let main argsArray =
    let dayNumber, args =
        match (argsArray |> Array.toList) with
        | dayNumString :: args -> (dayNumString |> int |> (flip (-) 1), args)
        | [] -> failwith "Please provide the day number as an argument"
    solutions[dayNumber] args
    0
