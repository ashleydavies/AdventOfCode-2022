module AdventOfCode.DayTwo

open System.IO
open FSharpx
open Microsoft.FSharp.Collections
open Helpers

type Hand =
    | Rock = 0
    | Paper = 1
    | Scissors = 2

let predeterminedLossCode = Hand.Rock
let predeterminedDrawCode = Hand.Paper
let predeterminedVictoryCode = Hand.Scissors

let whatWinsAgainst =
    (flip Map.find) (Map [ (Hand.Rock, Hand.Paper); (Hand.Paper, Hand.Scissors); (Hand.Scissors, Hand.Rock) ])

let baseScore = (flip Map.find) (Map [ (Hand.Rock, 1); (Hand.Paper, 2); (Hand.Scissors, 3) ])

let outcomeScore a b =
    if a = b then 3
    elif a = whatWinsAgainst b then 6
    else 0

let scoreRound = function 
    | elf :: [ player ] -> baseScore player + outcomeScore player elf
    | _ -> failwith "unknown round format"

let scoreGame = List.map scoreRound >> List.sum

let inputConversion =
    Map [ ("A", Hand.Rock)
          ("B", Hand.Paper)
          ("C", Hand.Scissors)
          ("X", Hand.Rock)
          ("Y", Hand.Paper)
          ("Z", Hand.Scissors) ]

// For the second scenario, Rock -> must lose, Paper -> must draw, Scissors -> Must win
let correctPlayerHand = function
    | elf :: [ r ] when r = predeterminedLossCode -> elf :: [ elf |> whatWinsAgainst |> whatWinsAgainst ]
    | elf :: [ r ] when r = predeterminedDrawCode -> elf :: [ elf ]
    | elf :: [ r ] when r = predeterminedVictoryCode -> elf :: [ elf |> whatWinsAgainst ]
    | _ -> failwith "unknown round format"

let parseLine =
    String.splitChar [| ' ' |]
    >> List.ofSeq
    >> List.map ((flip Map.find) inputConversion)

let solution (args: string list) =
    let lines =
        File.ReadAllText args.Head
        |> LineGrouping
        |> List.map parseLine
    
    printfn $"Score: %i{ (lines |> scoreGame) }"
    printfn $"Corrected score: %i{ (lines |> List.map correctPlayerHand |> scoreGame) }"
