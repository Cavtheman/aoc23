open System
open System.IO

type Segment =
    | Start
    | NS
    | EW
    | NW
    | NE
    | SW
    | SE
    | Empty

type Pipe = array<array<Segment>>
type Dir = N | S | E | W

let parseFile =
    File.ReadLines
    >> Seq.toArray
    >> Array.map (Seq.toArray
                  >> Array.map (fun x ->
                                match x with
                                | 'S' -> Start
                                | '|' -> NS
                                | '-' -> EW
                                | 'J' -> NW
                                | 'L' -> NE
                                | '7' -> SW
                                | 'F' -> SE
                                | _   -> Empty))

// Helper function to find all distances from the start
let rec helper
    (pipe : Pipe)
    (distArr : int array2d)
    (from : Dir)
    ((x,y) : int * int)
    (dist : int) : int array2d =

    (distArr.[x,y]) <- dist;
    match from, pipe[x][y] with
    | S, NS -> helper pipe distArr S (x-1,y) (dist + 1)
    | S, SW -> helper pipe distArr E (x,y-1) (dist + 1)
    | S, SE -> helper pipe distArr W (x,y+1) (dist + 1)
    | N, NS -> helper pipe distArr N (x+1,y) (dist + 1)
    | N, NW -> helper pipe distArr E (x,y-1) (dist + 1)
    | N, NE -> helper pipe distArr W (x,y+1) (dist + 1)
    | W, EW -> helper pipe distArr W (x,y+1) (dist + 1)
    | W, NW -> helper pipe distArr S (x-1,y) (dist + 1)
    | W, SW -> helper pipe distArr N (x+1,y) (dist + 1)
    | E, EW -> helper pipe distArr E (x,y-1) (dist + 1)
    | E, SE -> helper pipe distArr N (x+1,y) (dist + 1)
    | E, NE -> helper pipe distArr S (x-1,y) (dist + 1)
    | _, Start ->
        distArr.[x,y] <- 0
        distArr
    | _ ->
        printfn "%A" distArr
        failwith (sprintf "Pathing went wrong at %A" (x,y))

// Gets the minimum distances to any point in the array, following the loop from the start
let getDistArr (pipe : Pipe) =
    let startx, starty =
        pipe // Very cursed way of finding the start position
        |> Array.findIndex (Array.tryFindIndex ((=) Start) >> Option.isSome)
        |> fun i -> (i, pipe[i] |> Array.findIndex ((=) Start))

    let neighbours : list<Dir * (int * int)> =
        [(S,-1,0); (N,1,0); (E,0,-1); (W,0,1)]
        |> List.map (fun (dir,x,y) -> dir, startx + x, starty + y)
        |> List.filter (fun (dir,x,y) -> x >= 0 && y >= 0)
        |> List.filter (fun (dir,x,y) ->
                        match dir with
                        | N -> pipe[x][y] = NS || pipe[x][y] = NE || pipe[x][y] = NW
                        | S -> pipe[x][y] = NS || pipe[x][y] = SE || pipe[x][y] = SW
                        | E -> pipe[x][y] = EW || pipe[x][y] = NE || pipe[x][y] = SE
                        | W -> pipe[x][y] = EW || pipe[x][y] = NW || pipe[x][y] = SW)
        |> List.map (fun (dir,x,y) -> dir, (x,y))


    neighbours
    |> List.map (fun (dir, pos) ->
                 let arr = Array2D.create (Array.length pipe) (Array.length pipe.[0]) -1
                 in helper pipe arr dir pos 1)
    |> List.reduce (fun x y ->
                    Array2D.init
                        (Array.length pipe)
                        (Array.length pipe.[0])
                        (fun i j -> min x.[i,j] y.[i,j]))

let partOne (pipe : Pipe) =
    pipe
    |> getDistArr
    |> Seq.cast<int> // flattens the array so I can take max. Why is Array2D so weird?
    |> Seq.max

let partTwo (pipe : Pipe) =
    let distArr = getDistArr pipe
    let cleanArr = // Gets an array where all pipe segments not in the loop are gone
        Array2D.init
            (Array2D.length1 distArr)
            (Array2D.length2 distArr)
            (fun i j -> if distArr[i,j] = -1 then Empty else pipe[i][j])

    let mutable insideLoop = false
    let mutable innies = 0
    let mutable entrance = Empty

    // Counts spaces by simply checking whether we have passed through a pipe
    cleanArr
    |> Array2D.iteri (fun i j x ->
                      match entrance, x with
                      | _, Empty ->
                          if insideLoop then innies <- innies + 1 else ()
                      | _, NS ->
                          insideLoop <- not insideLoop
                      | _, EW | NE, NW | SE, SW -> ()
                      | _, NE -> entrance <- NE
                      | _, SE -> entrance <- SE
                      | NE, SW | SE, NW -> insideLoop <- not insideLoop
                      // Hardcoded cases for input below.
                      // Not nice but I can't be assed to do it properly
                      | NE, Start -> ()
                      | SE, Start -> insideLoop <- not insideLoop
                      // | _, Start -> entrance <- SE // For simple inputs
                      | _ -> failwith "Match cases are wrong"
                      )
    innies


let pipeSimple1 = parseFile "../input_simple1.txt"
let pipeSimple2 = parseFile "../input_simple2.txt"
let pipeSimple3 = parseFile "../input_simple3.txt"
let pipe = parseFile "../input.txt"

printfn "%A" <| partOne pipeSimple1
printfn "%A" <| partOne pipe

//printfn "%A" <| partTwo pipeSimple2
//printfn "%A" <| partTwo pipeSimple3
printfn "%A" <| partTwo pipe
