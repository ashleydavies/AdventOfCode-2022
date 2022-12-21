module AdventOfCode.DayTwentyOne

open System
open System.Diagnostics
open System.IO
open FSharpx
open Microsoft.FSharp.Collections
open Helpers
 
type Monkey =
    | MNum of int64
    | MAdd of string * string
    | MSub of string * string
    | MMul of string * string
    | MDiv of string * string
    | MEquality of string * string
    | MUnknown

// This is gross but Copilot generated the bulk of it for me and I'm tired and lazy
let parseMonkey line =
    let splitLine = line |> String.splitString [|": "|] StringSplitOptions.None
    let name = splitLine[0]
    let operation = splitLine[1]
    let monkey =
        if operation.Contains(" + ") then
            let split = operation |> String.splitString [| " + "|] StringSplitOptions.None
            MAdd(split[0], split[1])
        elif operation.Contains(" - ") then
            let split = operation |> String.splitString [| " - "|] StringSplitOptions.None
            MSub(split[0], split[1])
        elif operation.Contains(" * ") then
            let split = operation |> String.splitString [| " * "|] StringSplitOptions.None
            MMul(split[0], split[1])
        elif operation.Contains(" / ") then
            let split = operation |> String.splitString [| " / "|] StringSplitOptions.None
            MDiv(split[0], split[1])
        else
            MNum(int64 operation)
    (name, monkey)

let compute (lookup: Map<string, Monkey>) =
    let rec compute' = function
        | MNum(x) -> x
        | MAdd(x, y) -> compute' lookup[x] + compute' lookup[y]
        | MSub(x, y) -> compute' lookup[x] - compute' lookup[y]
        | MMul(x, y) -> compute' lookup[x] * compute' lookup[y]
        | MDiv(x, y) -> compute' lookup[x] / compute' lookup[y]
        | MEquality(x, y) -> failwith "Attempted to calculate with equality"
        | MUnknown -> failwith "Attempt to calculate with unknown"
    compute' lookup["root"]

type Computation =
    | Add of Computation * Computation
    | Sub of Computation * Computation
    | Mul of Computation * Computation
    | Div of Computation * Computation
    | Equality of Computation * Computation
    | Num of int64
    | Unknown

let rec monkeysToComputation(lookup: Map<string, Monkey>) = function
    | MAdd(x, y) -> Add(monkeysToComputation lookup lookup[x], monkeysToComputation lookup lookup[y])
    | MSub(x, y) -> Sub(monkeysToComputation lookup lookup[x], monkeysToComputation lookup lookup[y])
    | MMul(x, y) -> Mul(monkeysToComputation lookup lookup[x], monkeysToComputation lookup lookup[y])
    | MDiv(x, y) -> Div(monkeysToComputation lookup lookup[x], monkeysToComputation lookup lookup[y])
    | MEquality(x, y) -> Equality(monkeysToComputation lookup lookup[x], monkeysToComputation lookup lookup[y])
    | MNum(x) -> Num(x)
    | MUnknown -> Unknown

let rec symbolicCompute =
    let tryOperation left right operation =
       match (symbolicCompute left, symbolicCompute right) with
       | Some x, Some y -> Some (operation x y)
       | _ -> None
    function
    | Num(x) -> Some x
    | Add(x, y) -> tryOperation x y (+)
    | Sub(x, y) -> tryOperation x y (-)
    | Mul(x, y) -> tryOperation x y (*)
    | Div(x, y) -> tryOperation x y (/)
    | Unknown -> None
    | Equality(x, y) ->
        match (symbolicCompute x, symbolicCompute y) with
        | Some lhsResult, None ->
            match y with
            // Computation completed
            | Unknown -> Some lhsResult
            // Computation not completed - move symbols around
            | Add(a, b) ->
                match (symbolicCompute a, symbolicCompute b) with
                | Some num, None -> symbolicCompute (Equality((Num(lhsResult - num)), b))
                | None, Some num -> symbolicCompute (Equality((Num(lhsResult - num)), a))
                | _ -> None
            | Sub(a, b) ->
                match (symbolicCompute a, symbolicCompute b) with
                | Some num, None -> symbolicCompute (Equality((Num(-(lhsResult - num))), b))
                | None, Some num -> symbolicCompute (Equality((Num(lhsResult + num)), a))
                | _ -> None
            | Mul(a, b) ->
                match (symbolicCompute a, symbolicCompute b) with
                | Some num, None -> symbolicCompute (Equality((Num(lhsResult / num)), b))
                | None, Some num -> symbolicCompute (Equality((Num(lhsResult / num)), a))
                | _ -> None
            | Div(a, b) ->
                match (symbolicCompute a, symbolicCompute b) with
                | Some num, None -> symbolicCompute (Equality((Num(num / lhsResult)), b))
                | None, Some num -> symbolicCompute (Equality((Num(lhsResult * num)), a))
                | _ -> None
            | _ -> None
        | None, Some _ -> symbolicCompute (Equality(y, x))
        | Some x, Some y -> failwith "Equality had both sides defined -- unexpected"
        | _ -> failwith "Equality had no sides defined -- unexpected"

let solution (args: string list) =
    let monkeys = File.ReadAllText args.Head |> LineGrouping |> List.map parseMonkey |> Map.ofList
    printfn $"%d{compute monkeys}"
    let newRoot =
        match monkeys["root"] with
        | MAdd(x, y) -> MEquality(x, y)
        | _ -> failwith "Root monkey is not an addition"
    let updatedMonkeys = monkeys |> Map.add "humn" MUnknown
    printfn $"{symbolicCompute (monkeysToComputation updatedMonkeys newRoot)}"