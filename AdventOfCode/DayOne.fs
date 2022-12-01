module AdventOfCode.DayOne

open System
open System.IO
open FSharpx
open Microsoft.FSharp.Collections

let solution (args: string list) =
    let elfSums =
        File.ReadAllText(args.Head)
        |> String.splitString [|"\n\n"|] StringSplitOptions.None
        |> List.ofSeq
        |> List.map (String.splitChar[|'\n'|] >> List.ofSeq >> List.map int)
        |> List.map List.sum
        |> List.sortDescending
    let biggest = elfSums.Head
    let topThree = elfSums |> List.take 3 |> List.sum

    printfn $"Biggest: %i{biggest}; Top three: %i{topThree}"
