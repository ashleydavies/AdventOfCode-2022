module AdventOfCode.DayNine

open FSharpx.Text
open System.IO
open FSharpx
open Microsoft.FSharp.Collections
open System
open Helpers

type Vec2 = int * int

let Vec2Hof f a b =
    match (a, b) with
    | (ax, ay), (bx, by) -> (f ax bx, f ay by)
    
let Vec2Add = Vec2Hof (+)
let Vec2Sub = Vec2Hof (-)
let Vec2Clamp (maxUnit: int) = function
    | (ax, ay) -> (Math.Clamp(ax, -maxUnit, maxUnit), Math.Clamp(ay, -maxUnit, maxUnit))
let Vec2Closen a b = Vec2Sub b a |> Vec2Clamp 1 |> Vec2Add a

let Vec2Adjacent a b =
    match (a, b) with
    | (ax, ay), (bx, by) -> (max (abs(ax - bx)) (abs (ay - by))) <= 1

let directionMap =
    (flip Map.find) (Map [ ("U", (0, 1)); ("R", (1, 0)); ("D", (0, -1)); ("L", (-1, 0)) ])

let rec pullTails vHead = function
    | [] -> []
    | vTail :: vTails ->
        if Vec2Adjacent vHead vTail then
            vTail :: vTails
        else
            let vTail' = Vec2Closen vTail vHead
            vTail' :: pullTails vTail' vTails

let rec processDirection vHead vTails vDir = function
    | 0 -> (vHead, vTails, [vTails |> List.last] |> Set.ofList)
    | n ->
        let vHead' = Vec2Add vHead vDir
        let vTails' = pullTails vHead' vTails
        let vHead'', vTail'', vHistory = processDirection vHead' vTails' vDir (n - 1)
        (vHead'', vTail'', [vTails |> List.last] |> Set.ofList |> Set.union vHistory)

let rec processInstructions tailLength =
    let rec processInstructions' vHead vTails = function
    | [] -> Set.empty
    | (vDir, n) :: rest ->
        let vHead', vTails', vHistory = processDirection vHead vTails vDir n
        processInstructions' vHead' vTails' rest |> Set.union vHistory

    processInstructions' (0, 0) (List.init tailLength (fun _ -> (0, 0)))

let solution (args: string list) =
    let instructionLines = File.ReadAllText args.Head |> LineGrouping |> List.map (Strings.split ' ')
    let instructions = instructionLines |> List.map (fun s -> (directionMap s[0], int s[1]))
    
    printfn $"%d{processInstructions 1 instructions |> Set.count}"
    printfn $"%d{processInstructions 9 instructions |> Set.count}"
    ()
