open System
open System.IO

[<AbstractClass>]
type Hand () =
    abstract member cards : list<int>
    abstract member handRank : int

    interface IComparable with
        member this.CompareTo (hand : obj) : int =
            match hand with
            | :? Hand as compHand ->
                match compare this.handRank compHand.handRank with
                | 0 -> compare this.cards compHand.cards
                | other -> other
            | _ -> failwith "Hands cannot be compared to other types"

    // Not actually used
    override this.Equals (hand : obj) : bool =
        match hand with
        | :? Hand as compHand ->
            this.cards = compHand.cards
        | _ -> false

    // Compiler gives warning if this isn't here
    override this.GetHashCode () = failwith "Not implemented"

    // Purely for debugging
    override this.ToString () : string =
        (String.Concat (Array.ofList this.cards)).ToString()


// Hand class for part 1
type Hand1 (cards : list<char>) =
    inherit Hand ()

    let cardMap (cardChar : char) : int =
        match cardChar with
        | 'A' -> 14
        | 'K' -> 13
        | 'Q' -> 12
        | 'J' -> 11
        | 'T' -> 10
        | c -> int c - int '0'

    let rankHand (sortedHand : list<int>) : int =
        match sortedHand with
        | 5::xs    -> 7 // Five of a kind
        | 4::xs    -> 6 // Four of a kind
        | 3::2::xs -> 5 // Full house
        | 3::xs    -> 4 // Three of a kind
        | 2::2::xs -> 3 // Two pairs
        | 2::xs    -> 2 // One pair
        | 1::xs    -> 1 // High card
        | _ -> failwith "Hands not in correct format"

    override this.cards = cards |> List.map cardMap

    override this.handRank =
        this.cards
        |> List.countBy id
        |> List.map snd
        |> List.sortDescending
        |> rankHand


// Slightly different hand class for part 2
type Hand2 (cards : list<char>) =
    inherit Hand()
    let cardMap (cardChar : char) : int =
        match cardChar with
        | 'A' -> 14
        | 'K' -> 13
        | 'Q' -> 12
        | 'J' -> 1 // J is now weakest individual card
        | 'T' -> 10
        | c -> int c - int '0'

    // Ranking now has to take jokers into account
    let rankHand (sortedHand : list<int * int>) =
        let numJokers =
            match List.tryFind (fst >> ((=) 1)) sortedHand with
            | Some (_joker, n) -> n
            | None -> 0

        let handNoJs =
            sortedHand
            |> List.filter (fst >> (<>) 1)
            |> List.map snd

        match handNoJs, numJokers with
        | (n::xs, j) when n + j = 5 -> 7 // Five of a kind
        | ([], j)                   -> 7
        | (n::xs, j) when n + j = 4 -> 6 // Four of a kind
        | (3::2::xs, 0)             -> 5 // Full house
        | (2::2::xs, 1)             -> 5
        | (n::xs, j) when n + j = 3 -> 4 // Three of a kind
        | (2::2::xs, 0)             -> 3 // Two pairs
        | (n::xs, j) when n + j = 2 -> 2 // Pair
        | (n::xs, 0)                -> 1 // High Card
        | sortedHand, j -> failwith "Hands formatted incorrectly, or bug in ranking"

    override this.cards = cards |> List.map cardMap

    override this.handRank =
        this.cards
        |> List.countBy id
        |> List.sortByDescending snd
        |> rankHand


let parseFile1 =
    File.ReadLines
    >> Seq.toList
    >> List.map (fun x ->
                 x.Split ' '
                 |> fun y -> y[0] |> Seq.toList |> Hand1, y[1] |> int)

let parseFile2 =
    File.ReadLines
    >> Seq.toList
    >> List.map (fun x ->
                 x.Split ' '
                 |> fun y -> y[0] |> Seq.toList |> Hand2, y[1] |> int)


// Having implemented the IComparable interface, all that's left to do is sort the list
let partOne (allHands : list<Hand1 * int>) =
    allHands
    |> List.sortBy fst
    |> List.mapi (fun i x -> (i+1) * snd x)
    |> List.reduce (+)

let partTwo (allHands : list<Hand2 * int>) =
    allHands
    |> List.sortBy fst
    |> List.mapi (fun i x -> (i+1) * snd x)
    |> List.reduce (+)



let allHandsSimple1 = parseFile1 "../input_simple.txt"
let allHands1 = parseFile1 "../input.txt"

let allHandsSimple2 = parseFile2 "../input_simple.txt"
let allHands2 = parseFile2 "../input.txt"

printfn "%A" <| partOne allHandsSimple1
printfn "%A" <| partOne allHands1

printfn "%A" <| partTwo allHandsSimple2
printfn "%A" <| partTwo allHands2
