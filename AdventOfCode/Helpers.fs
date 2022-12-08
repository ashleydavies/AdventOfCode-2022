module AdventOfCode.Helpers

open System
open FSharpx
open Microsoft.FSharp.Collections

let MaxInt a b = if a > b then a else b
let ClampPositive = MaxInt 0

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
