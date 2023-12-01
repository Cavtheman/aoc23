open System
open System.IO

let partOne (filename : string) =
    let numbers = List.ofSeq "123456789"
    filename
    |> File.ReadLines
    |> Seq.toList
    |> List.map (String.filter (fun c -> List.contains c numbers)
                 >> List.ofSeq
                 >> List.map string)
    |> List.map (fun x -> (List.head x) + (List.last x) |> int)
    |> List.sum

let partTwo (filename : string) : int =
    let numbers =
        (List.ofSeq "1234567890" |> List.map (fun c -> String [|c|]))
        @ ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]

    // Gets tail substrings. So given "abc", it returns ["abc"; "bc"; "c"]
    let getSubStrings (s : string) : list<string> =
        List.init (String.length s) (fun i -> s[i..])

    // Finds all numbers in the given strings, both words and numerals
    let rec getNums (acc : string) (cList : list<char>) : list<string> =
        let matches =
            List.filter (fun (a,b) -> a = b) (List.allPairs (getSubStrings acc) numbers)
            |> List.map fst
        match matches, cList with
        | x::xs::xss, _ -> failwith "More than one match. Should not be possible."
        | [], c::cs -> getNums (acc + string c) cs
        | [x], cs -> x :: getNums (acc[1..]) cs
        | _, [] -> []

    let toNumeral (s : string) : string =
        match s with
        | "one"   -> "1"
        | "two"   -> "2"
        | "three" -> "3"
        | "four"  -> "4"
        | "five"  -> "5"
        | "six"   -> "6"
        | "seven" -> "7"
        | "eight" -> "8"
        | "nine"  -> "9"
        | s -> s

    filename
    |> File.ReadLines
    |> Seq.toList
    |> List.map (List.ofSeq >> (getNums "") >> (List.map toNumeral))
    |> List.map (fun (x : list<string>) -> (List.head x) + (List.last x) |> int)
    |> List.sum

printfn "%A" <| partOne "../simple_input1.txt"
printfn "%A" <| partOne "../input.txt"

printfn "%A" <| partTwo "../simple_input2.txt"
printfn "%A" <| partTwo "../input.txt"
