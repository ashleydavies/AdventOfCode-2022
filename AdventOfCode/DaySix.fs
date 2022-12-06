module AdventOfCode.DaySix

open System.IO
open FSharpx
open FSharpx.Collections
open FSharpx.Iteratee
open FSharpx.Text
open Microsoft.FSharp.Collections

let rec drop n list =
    match n with
    | 0 -> list
    | _ -> drop (n - 1) (List.tail list)

let solve desiredUnique =
    let rec loop index remaining =
        let uniqueCount = remaining |> List.take desiredUnique |> List.distinct |> List.length
        if uniqueCount = desiredUnique then
            index + desiredUnique
        else
            let skip = desiredUnique - uniqueCount in loop (index + skip) (remaining |> drop skip)
    loop 0

let solution (args: string list) =
    let input = File.ReadAllText args.Head |> Strings.toCharArray |> List.ofArray
    printf $"4: %d{ (solve 4 input) }; 14: %d{ (solve 14 input) }"
    
