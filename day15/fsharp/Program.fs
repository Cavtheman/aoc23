open System
open System.IO

type HASHMAP = array<list<string * int>>

let parseFile =
    File.ReadLines
    >> Seq.head
    >> fun x -> x.Split ','
    >> Array.toList

let HASH : string -> int =
    Seq.toList
    >> List.map int
    >> List.fold (fun acc elem -> ((acc + elem) * 17) % 256) 0

// Removes a lens from the HASHMAP, if it exists
let removeLens (map : HASHMAP) (label : string) : HASHMAP =
    let key = label |> HASH
    map.[key] <-
       List.foldBack
           (fun (mapLabel, mapLens) acc ->
            if label = mapLabel then acc
            else (mapLabel, mapLens) :: acc)
           map.[key]
           []
    map

// Adds or replaces a lens from the HASHMAP
let addLens (map : HASHMAP) (label : string) (lens : int) : HASHMAP =
    let key = label |> HASH
    if List.exists (fun (mapLabel, _) -> label = mapLabel) map.[key] then
        map.[key] <-
            List.foldBack
                (fun (mapLabel, mapLens) acc ->
                 if label = mapLabel then (label, lens) :: acc
                 else (mapLabel, mapLens) :: acc)
                map.[key]
                []
    else
        map.[key] <- map.[key] @ [label,lens]
    map

// Reads a single - or = instruction and updates the HASHMAP
let readInstruction (map : HASHMAP) (str : string) : HASHMAP =
    match str with
    | str when str.Contains '-' ->
        let label = str.[..str.Length - 2]
        removeLens map label
    | str when str.Contains '=' ->
        let label = str.[..str.Length - 3]
        let lens = (int str.[str.Length-1]) - (int '0')
        addLens map label lens
    | _ -> failwith "Parsed incorrectly, could not find - or = in string"

// Helper function to get the final result
let findBoxFocusingPower (boxNum : int) (box : list<string * int>) : int =
    box
    |> List.fold (fun (i, acc) (_, focalLength) -> (i+1, (boxNum+1) * i * focalLength + acc)) (1,0)
    |> snd


let partOne (initSeq : list<string>) =
    initSeq
    |> List.fold (fun acc elem -> acc + (HASH elem)) 0

let partTwo (initSeq : list<string>) =
    let hashmap = Array.create 256 []
    initSeq
    |> List.fold readInstruction hashmap
    |> Array.mapi findBoxFocusingPower
    |> Array.sum

let seqSimple = parseFile "../input_simple.txt"
let seq = parseFile "../input.txt"

printfn "%A" <| partOne seqSimple
printfn "%A" <| partOne seq

printfn "%A" <| partTwo seqSimple
printfn "%A" <| partTwo seq
