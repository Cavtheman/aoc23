

namespace FSharp


module Program

[<AbstractClass>]
type Hand =
    interface System.IComparable
    
    new: unit -> Hand
    
    override Equals: hand: obj -> bool
    
    override GetHashCode: unit -> int
    
    override ToString: unit -> string
    
    abstract cards: int list
    
    abstract handRank: int

type Hand1 =
    inherit Hand
    
    new: cards: char list -> Hand1
    
    member test: unit -> int
    
    override cards: int list
    
    override handRank: int

type Hand2 =
    inherit Hand
    
    new: cards: char list -> Hand2
    
    override cards: int list
    
    override handRank: int

val parseFile1: (string -> (Hand1 * int) list)

val parseFile2: (string -> (Hand2 * int) list)

val partTwo: allHands: ('a * int) list -> int when 'a: comparison

val allHandsSimple1: (Hand1 * int) list

val allHands1: (Hand1 * int) list

val allHandsSimple2: (Hand2 * int) list

val allHands2: (Hand2 * int) list

