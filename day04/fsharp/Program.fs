open System
open System.IO

type Game = list<int> * list<int>

let parseFile =
    let parseLine (line : string) : list<int> * list<int> =
        ((line.Split ':')[1]).Split '|' // Very ugly way of getting to the meat
        |> Array.toList
        |> List.map (fun x -> x.Split ' ' |> Array.toList |> List.except [""] |> List.map int)
        |> fun [have; winning] -> (have, winning) // Gives a warning, but if there are more than two elements, something is very wrong
    File.ReadLines
    >> Seq.toList
    >> List.map parseLine

let getWinningNumbers (have : list<int>) (winning : list<int>) : list<int> =
        winning
        |> List.filter (fun num -> List.exists ((=) num) have)

let partOne (games : list<Game>) =
    games
    |> List.map (fun (have, winning) ->
                 getWinningNumbers have winning
                 |> List.length
                 |> fun len -> 2. ** (len - 1 |> float)
                 |> int)
    |> List.sum

let partTwo (games : list<Game>) =
    let numWins =
        games
        |> List.map (fun (have, winning) ->
                     getWinningNumbers have winning
                     |> List.length)

    let totalCards = Array.create (List.length numWins) 1
    numWins // Iterates over the number of wins and adds cards to the total pile
    |> List.iteri (fun i x ->
                   for j in [(i+1)..(i+x)] do
                       totalCards[j] <- totalCards[j] + totalCards[i])

    totalCards
    |> Array.sum


let gamesSimple = parseFile "../input_simple.txt"
let games = parseFile "../input.txt"


printfn "%A" <| partOne gamesSimple
printfn "%A" <| partOne games

printfn "%A" <| partTwo gamesSimple
printfn "%A" <| partTwo games
