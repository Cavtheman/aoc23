open System
open System.IO

type Value =
    | Symbol
    | Number of int
type Cell = Value option


type Point = int * int
type Engine = array<array<Cell>>

let parseFile : string -> Engine =
    let parseCell (c : char) : Cell =
        match c with
        | '.' -> None
        | c when Char.IsDigit c -> Some (Number (int c - int '0')) // Probably stupid way of converting char to int properly
        | c -> Some Symbol
    File.ReadLines
    >> Seq.toArray
    >> Array.map (Array.ofSeq >> Array.map parseCell)

let findSymbolNeighbours (engine : Engine) (origin : Point) : list<int> =
    let neighbours =
        List.allPairs [-1..1] [-1..1]
        |> List.except [(0,0)]
        |> List.map (fun (a,b) -> a + fst origin, b + snd origin)

    // Finds the leftmost point of a sequence of numbers.
    // Assumes that original point is a number
    let rec extendLeft ((x,y) : Point) : option<Point> =
        if y < 0 then
            None
        else
            match engine[x][y] with
            | None | Some Symbol -> None
            | Some (Number _) ->
                match extendLeft (x,y-1) with
                | None -> Some (x,y)
                | Some result -> Some result

    // Given the leftmost coordinate of a number, finds the whole number
    let rec extendRight (acc : int) ((x,y) : Point) : int =
        if y >= Array.length engine then
            acc
        else
            match engine[x][y] with
            | None | Some Symbol -> acc
            | Some (Number n) -> extendRight (acc * 10 + n) (x,y+1)

    neighbours
    |> List.choose extendLeft
    |> List.distinct
    |> List.map (extendRight 0)

// Finds the coordinates of all symbols
let allSymbolPoints (engine : Engine) : list<Point> =
    let lineSymbols (line : array<Cell>) : array<int> =
        Array.indexed line |> Array.choose (fun (i, x) ->
                                            match x with
                                            | Some Symbol -> Some i
                                            | _ -> None)

    engine
    |> Array.mapi (fun i x -> lineSymbols x |> Array.map (fun j -> (i,j)))
    |> Array.concat
    |> Array.toList

let partOne (engine : Engine) =
    engine
    |> allSymbolPoints
    |> List.collect (findSymbolNeighbours engine)
    |> List.sum

let partTwo (engine : Engine) =
    engine
    |> allSymbolPoints
    |> List.map (findSymbolNeighbours engine)
    |> List.where (fun x -> List.length x = 2)
    |> List.map (List.reduce (*))
    |> List.sum

let smallEngine = parseFile "../input_simple.txt"
let bigEngine = parseFile "../input.txt"


printfn "%A" <| partOne smallEngine
printfn "%A" <| partOne bigEngine

printfn "%A" <| partTwo smallEngine
printfn "%A" <| partTwo bigEngine
