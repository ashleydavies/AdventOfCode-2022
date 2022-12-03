module AdventOfCode.DayThree

open System.IO
open FSharpx
open FSharpx.Collections
open FSharpx.Iteratee
open Microsoft.FSharp.Collections
open Helpers

let allLetters = ['A' .. 'Z'] |> List.append ['a' .. 'z']
let calcValue = ListIndex allLetters >> (+)1

let solveGrouping =
    List.sumBy (Set.intersectMany >> Set.map calcValue >> Set.fold (+) 0)

let solution (args: string list) =
    let lines =
        File.ReadAllText args.Head
        |> LineGrouping
        |> List.map (String.toCharArray >> List.ofArray)

    let sectionedGroups =
        lines
        |> List.map (List.splitInto 2 >> List.map Set.ofList)

    let tripleGroups =
        lines
        |> List.map Set.ofList
        |> List.chunkBySize 3

    printfn $"Priority sum: %i{sectionedGroups |> solveGrouping}"
    printfn $"Grouped sum: %i{tripleGroups |> solveGrouping}"
