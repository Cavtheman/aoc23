open System
open System.IO


let parseNode (line : string) =
    (line[..2], (line[7..9], line[12..14]))

let parseFile (filename : string) =
    let dirc, nodesTemp =
        filename
        |> File.ReadLines
        |> Seq.toList
        |> fun x -> x[0], x[2..]

    let dirs = dirc |> Seq.toList
    let firstNode = parseNode (nodesTemp[0])
    let nodes =
        nodesTemp
        |> List.map parseNode
        |> List.fold (fun acc (key, (left, right)) -> Map.add key (left,right) acc) (Map [])

    dirs, nodes

let partOne (dirs : list<char>) (nodes : Map<string, string * string>) =
    let rec helper (i : int) (curNode : string) (dirsLoop : list<char>) =
        match dirsLoop, Map.find curNode nodes with
        | ([],_) -> helper i curNode dirs
        | ('L'::xs, ("ZZZ", _)) -> i
        | ('R'::xs, (_, "ZZZ")) -> i
        | ('L'::xs, (left, _))  -> helper (i+1) left xs
        | ('R'::xs, (_, right)) -> helper (i+1) right xs
        | _ -> failwith "Should never happen"

    helper 1 "AAA" dirs



// Naive solution that simply loops through it all
// let partTwo (dirs : list<char>) (nodes : Map<string, string * string>) =
//     let isFinal : list<string> -> bool = List.forall (fun x -> x[2] = 'Z')
//     let rec helper (i : int) (curNodes : list<string>) (dirsLoop : list<char>) =
//         if isFinal curNodes then
//             i
//         else
//             match dirsLoop with
//             | [] ->
//                 helper i curNodes dirs
//             | 'L'::xs ->
//                 //printfn "%A" curNodes
//                 curNodes
//                 |> List.map (fun curNode -> Map.find curNode nodes |> fst)
//                 |> fun newNodes -> helper (i+1) newNodes xs
//             | 'R'::xs ->
//                 //printfn "%A" curNodes
//                 curNodes
//                 |> List.map (fun curNode -> Map.find curNode nodes |> snd)
//                 |> fun newNodes -> helper (i+1) newNodes xs
//             | _ -> failwith "Should never happen"
//     let starts = findStarts nodes
//     helper 0 starts dirs

// Lazy way of getting the first elements using the already existing map
let findStarts (nodes : Map<string, string * string>) : list<string> =
    nodes
    |> Map.filter (fun (key : string) v -> key[2] = 'A')
    |> Map.toList
    |> List.map fst


let findLoop (dirs : list<char>)
             (nodes : Map<string, string * string>)
             (start : string) : list<string> =
    let nextNode (dir : char) (node : string) =
        match dir with
        | 'L' -> Map.find node nodes |> fst
        | 'R' -> Map.find node nodes |> snd
        | _ -> failwith "Should never happen"

    let rec helper (acc : list<string>) (curNode : string) (dirsLoop : list<char>) : list<string> =
        match dirsLoop with
        | [] ->
            helper acc curNode dirs
        | x::_ when List.exists ((=) (nextNode x curNode)) acc ->
            let loopStart = nextNode x curNode
            let loopWithTail = List.rev (curNode :: acc)
            let loopStart = List.findIndex ((=) loopStart) loopWithTail
            loopWithTail[loopStart..]
        | x::xs ->
            helper (curNode :: acc) (nextNode x curNode) xs

    helper [] start dirs

let rec gcd (a : int64) (b : int64) =
    if b = 0 then abs a
    else gcd b (a % b)

let lcm (a : int64) (b : int64) = a * b / (gcd a b)

// Calulates the lcm of a list of numbers
let rec lcmList (acc : int64) (lst : list<int64>) =
    match lst with
    | x::xs -> lcmList (lcm acc x) xs
    | [] -> acc

let partTwo (dirs : list<char>) (nodes : Map<string, string * string>) =
    nodes
    |> findStarts
    |> List.map (findLoop dirs nodes >> List.length >> int64)
    // Very important to also include length of instructions in the lcm calculation
    |> lcmList (List.length dirs |> int64)


let dirsSimple1, nodesSimple1 = parseFile "../input_simple1.txt"
let dirsSimple2, nodesSimple2 = parseFile "../input_simple2.txt"
let dirsSimple3, nodesSimple3 = parseFile "../input_simple3.txt"

let dirs, nodes = parseFile "../input.txt"

printfn "%A" <| partOne dirsSimple1 nodesSimple1
printfn "%A" <| partOne dirsSimple2 nodesSimple2
printfn "%A" <| partOne dirs nodes

printfn "%A" <| partTwo dirsSimple3 nodesSimple3
printfn "%A" <| partTwo dirs nodes
