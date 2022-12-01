module AdventOfCode.Program

open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

let solutions = [
    DayOne.solution
]

[<EntryPoint>]
let main args =
    let argsList = args |> Array.toList
    let solutionNumber = args[0] |> int |> (-)1
    solutions[solutionNumber] argsList.Tail
    0
