open System
open System.IO

let parseFileOne =
    File.ReadLines
    >> Seq.toList
    >> List.map (fun x ->
                 x.Split ' '
                 |> Array.toList
                 |> List.tail
                 |> List.except [""]
                 |> List.map int)
    >> fun x -> List.zip x[0] x[1]

let parseFileTwo =
    File.ReadLines
    >> Seq.toList
    >> List.map (fun x ->
                 x.Split ' '
                 |> Array.toList
                 |> List.tail
                 |> List.except [""]
                 |> List.reduce (+)
                 |> float)
    >> fun x -> x[0], x[1]


// This will probably come back to bite me in the ass for part 2
let calcDists (time : int) =
    [1..time-1] // Using none of, or all the time makes no sense
    |> List.map (fun speed -> (time - speed) * speed)


// Race format is (time, dist)
let partOne (races : list<int * int>) =
    races
    |> List.map (fun (time, dist) ->
                 time |> calcDists |> List.filter ((<) dist) |> List.length)
    |> List.reduce (*)

// Part two is just a single solution, but with larger numbers so it can't be brute forced
// I should definitely just have done this first
let partTwo (time : float) (dist : float) =
    let quadraticSolver (pm : float -> float -> float) : float =
        (pm -time (sqrt (time**2. + 4. * -dist))) / -2.
    let s1 = quadraticSolver (+) |> ceil |> int
    let s2 = quadraticSolver (-) |> ceil |> int
    if s1 < s2 then s2 - s1 else s1 - s2


let racesSimple = parseFileOne "../input_simple.txt"
let races = parseFileOne "../input.txt"

let timeSimple, distSimple = parseFileTwo "../input_simple.txt"
let time, dist = parseFileTwo "../input.txt"

printfn "%A" <| partOne racesSimple
printfn "%A" <| partOne races

printfn "%A" <| partTwo timeSimple distSimple
printfn "%A" <| partTwo time dist
