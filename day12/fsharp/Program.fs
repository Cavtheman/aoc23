open System
open System.IO

let parseFile =
    File.ReadLines
    >> Seq.toList
    >> List.map (fun x ->
                 x.Split ' '
                 |> (fun [|springs; nums|] ->
                     List.ofSeq springs, nums.Split ',' |> Seq.toList |> List.map int))


printfn "%A" <| parseFile "../input_simple.txt"
