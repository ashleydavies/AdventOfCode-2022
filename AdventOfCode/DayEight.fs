module AdventOfCode.DayEight

open Helpers
open System.IO
open FSharpx
open Microsoft.FSharp.Collections

type Map = int[,]

let outOfBounds x y map =
    x < 0 || y < 0 || x >= (map |> Array2D.length1) || y >= (map |> Array2D.length2)

let processVisibilityDirection (map: Map) (minimumVisibilityMap: Map) idX idY deltaX deltaY =
    let rec processVisibilityDirection' maxSeen idX idY =
        if outOfBounds idX idY map then ()
        else 
            minimumVisibilityMap[idX, idY] <- min maxSeen minimumVisibilityMap[idX, idY]
            let maxSeen' = max maxSeen map[idX, idY]
            processVisibilityDirection' maxSeen' (idX + deltaX) (idY + deltaY)
            ()
    processVisibilityDirection' -1 idX idY

let calculateScenicScore (map: Map) x y =
    let baseTree = map[x, y]
    let calculateScenicScoreDirection curr idX idY dX dY =
        let rec calculateScenicScoreDirection' curr idX idY =
            if outOfBounds idX idY map then curr - 1
            else if outOfBounds idX idY map || map[idX, idY] >= baseTree then curr
            else calculateScenicScoreDirection' (curr + 1) (idX + dX) (idY + dY)
        calculateScenicScoreDirection' curr idX idY
    
    let directions = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]
    directions |> List.map (fun (dX, dY) -> calculateScenicScoreDirection 1 (x + dX) (y + dY) dX dY) |> List.fold (*) 1

let solution (args: string list) =
    let map = File.ReadAllText args.Head |> Strings.split '\n' |> Array.map String.toCharArray |> array2D |> Array2D.map (string >> int)
    
    let minimumVisibilityMap = Array2D.create (map.GetLength 0) (map.GetLength 1) 10
    for x in 0..(map.GetLength 0) - 1 do
        processVisibilityDirection map minimumVisibilityMap x 0 0 1
        processVisibilityDirection map minimumVisibilityMap x ((map.GetLength 1) - 1) 0 -1
    for y in 0..(map.GetLength 1) - 1 do
        processVisibilityDirection map minimumVisibilityMap 0 y 1 0
        processVisibilityDirection map minimumVisibilityMap ((map.GetLength 0) - 1) y -1 0

    let visibleTrees = minimumVisibilityMap |> Array2D.mapi (fun x y v -> v < map[x, y])
    printfn $"%d{visibleTrees |> Array2DFold (fun s v -> s + if v then 1 else 0) 0}"
    let scenicScores = visibleTrees |> Array2D.mapi (fun x y v -> calculateScenicScore map x y)
    printfn $"%d{scenicScores |> Array2DFold max 0}"
    ()
