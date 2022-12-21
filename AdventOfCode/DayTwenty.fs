module AdventOfCode.DayTwenty

open System
open System.IO
open Microsoft.FSharp.Collections
open Helpers
 
let loop size = function
    | -1 -> size - 1
    | n -> n % size

let solve (numbers: int64 array) mixTimes =
    let length = numbers.Length
    let whereTheyAreNow = Array.init length (fun i -> i)
    let whereTheyCameFrom = Array.init length (fun i -> i)
    
    let rec shiftIndex baseIndex =
        let startIndex = whereTheyAreNow[baseIndex]
        let number = numbers[baseIndex]
        let swap idx1 idx2 =
            let originIdx1 = whereTheyCameFrom[idx1]
            let originIdx2 = whereTheyCameFrom[idx2]
            let temp = whereTheyAreNow[originIdx1]
            // Update our reverse references
            whereTheyAreNow[originIdx1] <- whereTheyAreNow[originIdx2]
            whereTheyAreNow[originIdx2] <- temp
            // Update our indexes
            whereTheyCameFrom[idx1] <- originIdx2
            whereTheyCameFrom[idx2] <- originIdx1
            
        let rec move idx = function
            | 0 ->
                ()
            | n ->
                let delta = Math.Clamp(n, -1, 1)
                let idx' = loop length (idx + delta)
                swap idx idx'
                move idx' (n - delta)
        let moveLength = int (number % (int64 length - 1L))
        move startIndex moveLength
    
    for t in 1 .. mixTimes do
        for i in 0 .. length - 1 do
            shiftIndex i
 
    let zeroPosition = whereTheyAreNow[Array.findIndex (fun i -> i = 0L) numbers]
    let lookupExtraIdx idx = numbers[whereTheyCameFrom[(zeroPosition + idx) % length]]
    printfn $"Sum: {lookupExtraIdx 1000 + lookupExtraIdx 2000 + lookupExtraIdx 3000}"
    

let solution (args: string list) =
    let numbers = File.ReadAllText args.Head |> LineGrouping |> List.map int64 |> Array.ofList
    
    solve numbers 1
    solve (numbers |> Array.map (fun x -> x * 811589153L)) 10
    ()
