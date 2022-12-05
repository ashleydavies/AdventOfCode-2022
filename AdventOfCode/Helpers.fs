module AdventOfCode.Helpers

open System
open FSharpx
open Microsoft.FSharp.Collections

let LineGrouping =
    String.splitChar [| '\n' |] >> List.ofSeq

let BlockGrouping =
    String.splitString [| "\n\n" |] StringSplitOptions.None
    >> List.ofSeq
    >> List.map LineGrouping

let NumericBlockGrouping = BlockGrouping >> List.map (List.map int)

let ListIndex list =
    (=)
    >> (flip List.findIndex) list

let ArrayGet idx = flip Array.get idx
let (<&>) f g x = f x && g x
let (<|>) f g x = f x || g x
let (>&<) x (b, c) = x >= b && x <= c
