module AdventOfCode.DayFive

open System.IO
open System.Text.RegularExpressions
open FSharpx
open FSharpx.Collections
open FSharpx.Iteratee
open FSharpx.Text
open Microsoft.FSharp.Collections
open Helpers

let parseInstruction instruction =
    Regex.Match(instruction, ".+?(\d+).+?(\d+).+?(\d+)").Groups
    |> Seq.toList
    |> List.tail
    |> List.map (fun f -> f.Value)
    |> List.map int

let solve startingStacks cranePickupModifier =
    let rec solveRec (currentStacks: char list []) = function
        | currentInstruction :: remainingInstructions ->
            match parseInstruction currentInstruction with
            | count :: fromCol :: [ toCol ] ->
                let toCol, fromCol = toCol - 1, fromCol - 1
                let movedElements, newFromStack = currentStacks.[fromCol] |> List.splitAt count
                let newToStack = currentStacks.[toCol] |> List.append (movedElements |> cranePickupModifier)
                let newStacks = currentStacks |> Array.updateAt toCol newToStack |> Array.updateAt fromCol newFromStack
                solveRec newStacks remainingInstructions
            | _ -> failwith $"Failed to parse instruction"
        | [] -> currentStacks
    solveRec startingStacks >> Array.map List.head >> System.String

let parseInitialCrates columnPositions startingCrates =
    let reversedCrates = startingCrates |> List.rev |> List.map Strings.toCharArray
    columnPositions |> Array.map (fun position ->
        reversedCrates
        |> List.map (ArrayGet position)
        |> List.fold (fun acc char -> if char = ' ' then acc else char :: acc) []
    )

let parseColumnPositions (columnString: string) =
    columnString.Split(' ')
    |> Array.filter ((<>) "")
    |> Array.map ((flip String.indexOfString) columnString)

let solution (args: string list) =
    match (File.ReadAllText args.Head
           |> LineGrouping
           |> List.split (String.contains "1")) with
    | startingCrates, columnHeaders :: _ :: instructions ->
        let columnPositions = parseColumnPositions columnHeaders
        printfn $"CrateMover 9000: %s{solve (parseInitialCrates columnPositions startingCrates) List.rev instructions}"
        printfn $"CrateMover 9001: %s{solve (parseInitialCrates columnPositions startingCrates) id instructions}"
    | _ -> failwith "Invalid input"
