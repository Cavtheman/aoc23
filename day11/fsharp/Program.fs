open System
open System.IO

let parseFile =
    File.ReadLines
    >> Seq.toList
    >> List.mapi
        (fun i ->
         Seq.toList
         >> List.fold
             (fun (j, acc) x -> if x = '#' then (j+1L, (int64 i, int64 j) :: acc) else (j+1L, acc))
             (0L, []))
    >> List.collect snd

// Takes a universe, consisting of galaxy coordinates, and an expansion factor determining how much the empty space expands
let partOne (universe : list<int64*int64>) (expansionFactor : int64) =
    // Map from axis coordinates to expansion amount for each element in axis
    let getExpansions (fullAxes : list<int64>) : Map<int64,int64> =
        fullAxes
        |> List.fold
            (fun (i, acc) x ->
             (i+1L, (x, (expansionFactor - 1L) * (x - i)) :: acc))
             (0L, [])
        |> snd
        |> Map

    let yExpansions =
        universe
        |> List.map fst
        |> List.distinct
        |> List.sort
        |> getExpansions

    let xExpansions =
        universe
        |> List.map fst
        |> List.distinct
        |> List.sort
        |> getExpansions

    universe
    |> List.map (fun (y,x) -> y + yExpansions[y], x + xExpansions[x])
    |> fun x -> List.allPairs x x |> List.filter (fun (y1, y2) -> y1 <> y2)
    |> List.map (fun ((x1,x2), (y1,y2)) -> abs (x1 - y1) + abs (x2 - y2))
    |> List.sum
    |> fun x -> x / 2L

let universeSimple = parseFile "../input_simple.txt"
let universe = parseFile "../input.txt"

printfn "%A" <| partOne universeSimple 2L
printfn "%A" <| partOne universe 2L

printfn "%A" <| partOne universeSimple 10L
printfn "%A" <| partOne universe 1000000L
