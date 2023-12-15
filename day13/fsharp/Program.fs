open System
open System.IO

type Stuff =
    | Ashs
    | Rock

type Field = array<array<Stuff>>

type MirrorLine = int * int


let parseFile : string -> list<Field> =
    File.ReadLines
    >> Seq.toList
    >> fun x -> List.foldBack (fun x (acc::accs) -> if x = "" then []::acc::accs else (x::acc)::accs) x [[]]
    >> List.map (Array.ofList >> Array.map (Array.ofSeq >> Array.map (fun x -> if x = '#' then Rock else Ashs)))


let isMirror (field : Field) ((leftI, rightI) : MirrorLine) : bool =
    let left = field[..leftI] |> Array.rev
    let right = field[rightI..]
    let minL = min (Array.length left) (Array.length right) - 1
    left[..minL] = right[..minL]

let findMirror (field : Field) : bool * MirrorLine =
    let pairsX = [for x in [0..Array.length field - 2] do yield (x,x+1)]
    let pairsY = [for x in [0..Array.length field[0] - 2] do yield (x,x+1)]
    let xResult = List.tryFind (isMirror field) pairsX
    let yResult = List.tryFind (isMirror (Array.transpose field)) pairsY
    match xResult, yResult with
    | Some line, None -> (false, line)
    | None, Some line -> (true, line)
    | _ -> failwith "Could not find mirror point"

let isSmudgedMirror (field : Field) ((leftI, rightI) : MirrorLine) : bool =
    let left = field[..leftI] |> Array.rev
    let right = field[rightI..]
    let minL = min (Array.length left) (Array.length right) - 1
    let equals (left : array<Stuff>) (right : array<Stuff>) : int =
        //printfn "%A\n%A" left right
        (left, right)
        ||> Array.map2 (fun l r -> if l = r then 1 else 0)
        |> Array.sum
    //printfn "minl %A" minL
    //printfn "%A\n\n%A" left[..minL] right[..minL]
    (left[..minL], right[..minL])
    ||> Array.map2 equals
    //|> fun x -> printfn "%A" x; x
    |> Array.fold (fun (acc, oneWrong) x ->
                   if x = Array.length left[0] then (acc, oneWrong)
                   elif oneWrong || x < Array.length left[0] - 1 then (false, oneWrong)
                   else (acc, true)) (true,false)

    //|> fun x -> printfn "%A" x; x
    |> fun (x1, x2) -> x1 && x2
    //|> fst

let findSmudgedMirror (field : Field) : bool * MirrorLine =
    let pairsX = [for x in [0..Array.length field - 2] do yield (x,x+1)]
    let pairsY = [for x in [0..Array.length field[0] - 2] do yield (x,x+1)]
    //printfn "%A" pairsX
    //printfn "%A" pairsY
    let xResult = List.tryFind (isSmudgedMirror field) pairsX
    let yResult = List.tryFind (isSmudgedMirror (Array.transpose field)) pairsY
    //printfn "%A" <| (xResult, yResult)
    match xResult, yResult with
    | Some line, None -> (false, line)
    | None, Some line -> (true, line)
    | Some line1, Some line2 ->
        printfn "Found second mirror point %A" line2
        (false, line1)
    | _ -> failwith "Could not find mirror point"


let partOne (fields : list<Field>) =
    fields
    |> List.map (findMirror >> fun (transpose, (x1,_)) -> if transpose then x1+1 else 100*(x1+1))
    |> List.sum

let partTwo (fields : list<Field>) =
    fields
    |> List.map (findSmudgedMirror >> fun (transpose, (x1,_)) -> if transpose then x1+1 else 100*(x1+1))
    |> List.sum


let fieldsSimple = parseFile "../input_simple.txt"
let fields = parseFile "../input.txt"

printfn "%A" <| partOne fieldsSimple
printfn "%A" <| partOne fields

printfn "%A" <| partTwo fieldsSimple
printfn "%A" <| partTwo fields
