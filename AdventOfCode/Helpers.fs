module AdventOfCode.Helpers

open System
open FSharpx
open Microsoft.FSharp.Collections

let MaxInt = max 
let ClampPositive = MaxInt 0

let min3 x = min x >> min

let LineGrouping =
    String.splitChar [| '\n' |] >> List.ofSeq

let BlockGrouping =
    String.splitString [| "\n\n" |] StringSplitOptions.None
    >> List.ofSeq
    >> List.map LineGrouping

let NumericBlockGrouping = BlockGrouping >> List.map (List.map int)

let (|BeginsWith|_|) (prefix: string) (s: string) =
    if s.StartsWith prefix then Some (s.Substring prefix.Length) else None

let ListIndex list =
    (=)
    >> (flip List.findIndex) list

let ArrayGet idx = flip Array.get idx
let (<&>) f g x = f x && g x
let (<|>) f g x = f x || g x
let (>&<) x (b, c) = x >= b && x <= c

let Array2DFold (folder: 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    for x in 0 .. Array2D.length1 array - 1 do
        for y in 0 .. Array2D.length2 array - 1 do
            state <- folder state (array.[x, y])
    state
