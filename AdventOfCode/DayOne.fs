module AdventOfCode.DayOne

open System.IO
open Microsoft.FSharp.Collections
open Helpers

let solution (args: string list) =
    let elfSums =
        File.ReadAllText args.Head
        |> NumericBlockGrouping
        |> List.map List.sum
        |> List.sortDescending

    printfn $"Biggest: %i{elfSums.Head}; Top three: %i{(elfSums |> List.take 3 |> List.sum)}"
