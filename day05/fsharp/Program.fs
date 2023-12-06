open System
open System.IO

type Range = int64 * int64
type RangeMap = int64 * int64 * int64

let parseFile (filename : string) =
    let splitList (sep : 'a) (lst : list<'a>) : list<list<'a>> =
        List.foldBack (fun elem (x::xs) -> // Gives warning, but the match will always work
                       if elem = sep then [] :: x :: xs
                       else (elem :: x) :: xs) lst [[]]

    let strToRangeFun (str : string) : (int64 -> int64) =
        str.Split ' '
        |> Array.map int64
        |> fun [|dest; source; range|] -> // Gives warning, but the match will always work
            fun x ->
                if x >= source && x < source + range then x - source + dest
                else x

    let strToRangeNum (str : string) : RangeMap =
        str.Split ' '
        |> Array.map int64
        |> fun [|dest; src; range|] -> // Gives warning, but the match will always work
            src, dest, range


    let (seeds, maps) =
        filename
        |> File.ReadLines
        |> Seq.toList
        |> fun x -> List.head x, List.tail x

    let mapFuns : list<list<int64 -> int64>> =
        maps
        |> splitList ""
        |> List.except [[]]
        |> List.map (List.tail >> List.map strToRangeFun)

    let mapNums : list<list<RangeMap>> =
        maps
        |> splitList ""
        |> List.except [[]]
        |> List.map (List.tail >> List.map strToRangeNum)

    let seedVals =
        (seeds[7..]).Split ' ' // Removes the "seeds: " part of the string
        |> Array.map int64
        |> Array.toList

    seedVals, mapFuns, mapNums

let partOne (seeds : list<int64>) (allMaps : list<list<int64 -> int64>>) =
    // Assumes that a seed only has one valid map in a given set
    let fromRanges (seed : int64) (maps : list<int64 -> int64>) : int64 =
        List.fold (fun acc map -> if acc = seed then map seed else acc) seed maps

    seeds
    |> List.map (fun seed ->
                 allMaps
                 |> List.fold (fun acc maps ->
                               fromRanges acc maps) seed)
    |> List.min


// My function names for all of these are horrible
// output is in a (mapped, unchanged) format
// Given a single map from source to destination, and a single range of seeds, outputs the resulting list of ranges
let rec singleMap ((src, dest, range) : RangeMap) ((rSrc, rRange) : Range) : list<Range> * list<Range> =
    if rSrc >= src + range || rSrc + rRange <= src then
        ([], [(rSrc, rRange)])
    elif rSrc >= src && rSrc + rRange <= src + range then
        ([(rSrc - src + dest, rRange)], [])
    elif rSrc >= src then
        let newRange = range + src - rSrc
        let (mapped, unchanged) = singleMap (src, dest, range) (src + range, rRange - newRange)
        in ((rSrc - src + dest, newRange) :: mapped, unchanged)
    else // rSrc + rRange <= src + range
        let newRange = rRange - src + rSrc
        let (mapped, unchanged) = singleMap (src, dest, range) (rSrc, src - rSrc)
        in ((dest, newRange) :: mapped, unchanged)

// There are several mapping groups, where each individual mapping has to happen in parallel
let multiMap (rangeMaps : list<RangeMap>) (range : Range) : list<Range> =
    rangeMaps
    |> List.fold (fun (mapped, unchanged) map ->
                  List.map (singleMap map) unchanged
                  |> List.unzip
                  |> fun (x,y) -> mapped @ List.concat x, List.concat y
                  ) ([], [range])
    |> fun (mapped, unchanged) -> mapped @ unchanged

// Several mapping groups have to occur sequentially
let mapFull (allMaps : list<list<RangeMap>>) (range : Range) : list<Range> =
    allMaps
    |> List.fold (fun ranges rangeMaps ->
                  List.collect (multiMap rangeMaps) ranges)
                  [range]

let rec toPairs (seeds : list<int64>) : list<Range> =
    match seeds with
    | x::xs::xss -> (x,xs) :: toPairs xss
    | [] -> []
    | _ -> failwith "Parsing has gone wrong somehow"

let partTwo (seeds : list<int64>) (allMaps : list<list<RangeMap>>) =
    seeds
    |> toPairs
    |> List.collect (mapFull allMaps)
    |> List.map fst
    |> List.min

let seedsSimple, mapsSimple, mapNumsSimple = parseFile "../input_simple.txt"
let seeds, maps, mapNums = parseFile "../input.txt"

printfn "%A" <| partOne seedsSimple mapsSimple
printfn "%A" <| partOne seeds maps


printfn "%A" <| partTwo seedsSimple mapNumsSimple
printfn "%A" <| partTwo seeds mapNums
