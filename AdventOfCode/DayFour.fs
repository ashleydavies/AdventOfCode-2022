module AdventOfCode.DayFour

open System.IO
open FSharpx
open FSharpx.Collections
open FSharpx.Iteratee
open Microsoft.FSharp.Collections
open Helpers


let biggerStarter = ArrayGet 0
let smallerStarter = ArrayGet 1

// Returns pairs, where individual pairs are sorted by size of assignments
let parseLine =
    String.splitChar [| ',' |]
    >> Array.map (String.splitChar [| '-' |] >> Array.map int)
    >> Array.map (fun x -> (x.[0], x.[1]))
    >> Array.sortByDescending (fun x -> snd x - fst x)

let overlapStart x =
    (smallerStarter >> fst) x >&< biggerStarter x

let overlapEnd x =
    (smallerStarter >> snd) x >&< biggerStarter x

let solution (args: string list) =
    let input =
        File.ReadAllText args.Head
        |> LineGrouping
        |> List.map parseLine
    
    printfn $"Count of overlapping: %i{ input |> List.filter (overlapStart <&> overlapEnd) |> List.length}"
    printfn $"Count of partially overlapping: %i{ input |> List.filter (overlapStart <|> overlapEnd) |> List.length}"
