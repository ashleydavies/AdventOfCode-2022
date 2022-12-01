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
