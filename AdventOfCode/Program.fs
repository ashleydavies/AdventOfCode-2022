module AdventOfCode.Program

open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open FSharpx

let solutions =
    [ DayOne.solution
      DayTwo.solution
      DayThree.solution
      DayFour.solution
      DayFive.solution
      DaySix.solution
      DaySeven.solution
      DayEight.solution
      DayNine.solution
      DayTen.solution
      DayEleven.solution
      DayTwelve.solution
      DayThirteen.solution
      DayFourteen.solution
      DayFifteen.solution
      DaySixteen.solution
      DaySeventeen.solution
      DayEighteen.solution
      DayNineteen.solution
      DayTwenty.solution
      DayTwentyOne.solution ]

[<EntryPoint>]
let main argsArray =
    let dayNumber, args =
        match (argsArray |> Array.toList) with
        | dayNumString :: args -> (dayNumString |> int |> (flip (-) 1), args)
        | [] -> failwith "Please provide the day number as an argument"

    solutions[dayNumber] args
    0
