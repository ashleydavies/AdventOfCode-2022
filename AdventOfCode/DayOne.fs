module AdventOfCode.DayOne

open System.IO
open Microsoft.FSharp.Collections
open Helpers

let solution (args: string list) =
    let elfSums =
        File.ReadAllText(args.Head)
        |> NumericBlockGrouping
        |> List.map List.sum
        |> List.sortDescending

    let biggest = elfSums.Head
    let topThree = elfSums |> List.take 3 |> List.sum

    printfn $"Biggest: %i{biggest}; Top three: %i{topThree}"
