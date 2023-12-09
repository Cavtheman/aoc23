open System
open System.IO

let parseFile =
    File.ReadLines
    >> Seq.toList
    >> List.map (fun x -> x.Split ' ' |> Array.toList |> List.map int)

// Finds the list of differences between numbers in the list
let rec findDiffs (nums : list<int>) : list<int> =
        match nums with
        | x::xs::xss -> (xs - x) :: (findDiffs (xs::xss))
        | x::xs -> []
        | [] -> []

// Extrapolates the differences to find next number
let rec findNext (nums : list<int>) : int =
    if List.forall ((=) 0) nums then
        0
    else
        let diffs = findDiffs nums
        let diff = findNext diffs
        in (List.last nums) + diff

// Extrapolates the differences to find previous number
let rec findPrev (nums : list<int>) =
    if List.forall ((=) 0) nums then
        0
    else
        let diffs = findDiffs nums
        let diff = findPrev diffs
        in (List.head nums) - diff

let partOne (allNums : list<list<int>>) : int =
    allNums
    |> List.map findNext
    |> List.sum

let partTwo (allNums : list<list<int>>) : int =
    allNums
    |> List.map findPrev
    |> List.sum

let inputSimple = parseFile "../input_simple.txt"
let input = parseFile "../input.txt"

printfn "%A" <| partOne inputSimple
printfn "%A" <| partOne input

printfn "%A" <| partTwo inputSimple
printfn "%A" <| partTwo input
