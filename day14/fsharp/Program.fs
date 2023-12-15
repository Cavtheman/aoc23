open System
open System.IO

type Cell =
    | Round
    | Cubed
    | Empty

type Platform = list<list<Cell>>

let parseFile =
    File.ReadLines
    >> Seq.toList
    >> List.map (Seq.toList >> List.map (fun x ->
                                         match x with
                                         | '#' -> Cubed
                                         | 'O' -> Round
                                         | _ -> Empty))
    >> List.transpose // Makes tilting north easier

let calcLoad (platform : Platform) : int =
    let maxLoad = List.length platform
    platform
    |> List.transpose
    |> List.mapi
        (fun i line ->
         (maxLoad - i) * (List.fold (fun acc elem ->
                                     if elem = Round then 1 + acc else acc) 0 line))
    |> List.sum

// Only a north title is required if we can also rotate the platform
let tiltNorth (platform : Platform) : Platform =
    let nRound n = List.replicate n (Round)
    let nEmpty n = List.replicate n (Empty)
    platform
    |> List.map
        (fun column ->
         List.foldBack (fun elem (acc, numRound, numEmpty) ->
                        match elem with
                        | Cubed ->
                            (Cubed :: (nRound numRound) @ (nEmpty numEmpty) @ acc, 0, 0)
                        | Round -> (acc, numRound + 1, numEmpty)
                        | Empty -> (acc, numRound, numEmpty + 1)
                        ) column ([], 0, 0)
         |> fun (acc, numRound, numEmpty) -> (nRound numRound) @ (nEmpty numEmpty) @ acc)




// Tilts in each direction once
let cycleOnce (platform : Platform) =
    let rec rotate acc platform =
        match platform with
        | [] -> failwith "Cannot rotate empty platform"
        | x when List.isEmpty (List.head x) -> acc
        | x -> // Ensures that we only recurse on a list of nonempty lists
            rotate' (List.map List.head platform :: acc) (List.map List.tail platform)

    // Tilts then rotates counterclockwise to simulate tilting in NWSE order
    let rec cycleTilt (n : int) (platform : Platform) =
        if n > 0 then
            platform
            |> tiltNorth
            |> rotate []
            |> cycleTilt (n-1)
        else
            platform

    cycleTilt 4 platform

let rec cycleN (n : int) (platform : Platform) =
    if n > 0 then cycleN (n-1) (cycleOnce platform)
    else platform

// Finds a cycle in the platform states
let findCycle (platform : Platform) =
    let rec helper (i : int) (acc : Set<Platform>) (platform : Platform) =
        if Set.contains platform acc then platform
        else
            platform
            |> cycleOnce
            |> helper (i+1) (Set.add platform acc)

    helper 0 (Set []) platform

let partOne (platform : Platform) =
    platform
    |> tiltNorth
    |> calcLoad

let partTwo (n : int) (platform : Platform) =
    let cycleStartPlatform = findCycle platform
    let rec findDistToStart i platform =
        if platform = cycleStartPlatform then i
        else cycleOnce platform |> findDistToStart (i+1)

    let cycleStart = findDistToStart 0 platform
    let cycleLength = findDistToStart 1 (cycleOnce cycleStartPlatform)
    let rotations = cycleStart + ((n - cycleStart) % cycleLength)

    platform
    |> cycleN rotations
    |> calcLoad

let platformSimple = parseFile "../input_simple.txt"
let platform = parseFile "../input.txt"

printfn "%A" <| partOne platformSimple
printfn "%A" <| partOne platform

printfn "%A" <| partTwo 1000000000 platformSimple
printfn "%A" <| partTwo 1000000000 platform
