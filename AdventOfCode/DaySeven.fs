module AdventOfCode.DaySeven

open System.IO
open FSharpx
open FSharpx.Collections
open Microsoft.FSharp.Collections
open Helpers

type FileNode =
    | Directory of Map<string, FileNode>
    | File of int

let parseFileListing =
    String.splitChar [| ' ' |]
    >> function
        [| size; name |] -> (name, File(int size))
        | _ -> failwith "Invalid file listing"

let rec insertDirectory newDirectory pointer =
    function
    | Directory contents ->
        match pointer with
        | [ dir ] -> Directory(contents |> Map.add dir newDirectory)
        | dir :: pointer' ->
            Directory(Map.updateWith (insertDirectory newDirectory pointer' >> Some) dir contents)
        | _ -> newDirectory
    | _ -> failwith "Cannot add to a file"


let rec buildFilesystem fs pointer =
    let notDirectory = not << Strings.startsWith "dir"
    
    function
    | command :: commands ->
        match command with
        | BeginsWith "$ cd " rest ->
            match rest with
            | "/" -> buildFilesystem fs [] commands
            | ".." -> buildFilesystem fs (List.tail pointer) commands
            | dirName -> buildFilesystem fs (dirName :: pointer) commands
        | "$ ls" ->
            let fileListing, commands' =
                commands
                |> List.split (Strings.startsWith "$")
            let files =
                fileListing
                |> List.filter notDirectory
                |> List.map parseFileListing
                |> Map.ofList
            buildFilesystem (fs |> insertDirectory (Directory files) (pointer |> List.rev)) pointer commands'
        | _ -> failwith $"Unknown command {command}"
    | [] -> fs

let rec foldSize accumulator = function
    | Directory items ->
        let results = items |> Map.valueList |> List.map (foldSize accumulator)
        let mySize, acc = results |> List.unzip |> fun (a, b) -> (a |> List.sum, b |> List.fold accumulator 0)
        (mySize, accumulator acc mySize)
    | File size -> (size, 0)

let solution (args: string list) =
    let fs = File.ReadAllText args.Head |> LineGrouping |> buildFilesystem (Directory Map.empty) []

    let totalSize, accSize = foldSize (fun acc e -> if e < 100000 then acc + e else acc) fs
    let minDelete = totalSize - 40000000
    let _, delete = foldSize (fun acc e -> if e >= minDelete && (e < acc || acc = 0) then e else acc) fs

    printfn $"%A{fs}"
    printfn $"%d{totalSize} %d{accSize}"
    printfn $"You can delete a dir of size: %d{delete}"
