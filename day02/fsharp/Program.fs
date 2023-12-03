open System
open System.IO

type Set = list<int * string>
type Game = list<Set>

let parseFile : string -> list<Game> =
    let gameParser (gameStr : string) =
        ((gameStr.Split ':')[1]).Split ';' // Very ugly way of getting to the meat
        |> Array.map (fun x ->
                      x.Split ','
                      |> Array.map (fun y ->
                                    let temp = y.Split ' ' in
                                    (temp[1] |> int, temp[2])))
                                    // This entire thing is just very ugly
        |> Array.map Array.toList
        |> Array.toList

    File.ReadLines
    >> Seq.toList
    >> List.map gameParser

let partOne (limits : Map<string, int>) (games : list<Game>) : int =
    // Figures out whether a game is good
    let goodGame : Game -> bool =
        List.concat >> List.forall (fun (n, col) -> n <= limits[col])

    games
    |> List.map goodGame
    |> List.fold (fun (i, acc) good ->
                  if good then i + 1, acc + i else i + 1, acc) (1, 0)
    |> snd

let partTwo (games : list<Game>) =
    let emptyGame = Map [("red", 0); ("green", 0); ("blue", 0)]
    let gamePower : Game -> int =
       List.concat
       >> List.fold (fun (acc : Map<string, int>) (n, col) -> // Type annotation is necessary for it to compile for some reason
                     if n > acc[col] then Map.add col n acc else acc) emptyGame
       >> Map.fold (fun acc _ n -> acc * n) 1
    games
    |> List.map gamePower
    |> List.sum


let simpleGames = parseFile "../input_simple.txt"
let fullGames = parseFile "../input.txt"


let limits = Map [ ("red", 12); ("green", 13); ("blue", 14) ]
printfn "%A" <| partOne limits simpleGames
printfn "%A" <| partOne limits fullGames

printfn "%A" <| partTwo simpleGames
printfn "%A" <| partTwo fullGames
