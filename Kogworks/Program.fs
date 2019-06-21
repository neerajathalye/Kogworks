open System

type CellState = 
| R // Red
| B // Blue
| G // Golden
| E // Empty

type GameStatus = 
| InProgress
| WonByB
| WonByR 


let jagged = [| for a in 1 .. 10 do yield [| for a in 1 .. a do yield E |] |] // create a 10x10 jagged array with increasing number of pegs on each row and assign them empty

jagged.[0].[0] <- G // Assign the top corner golden
jagged.[9].[0] <- B // Assign the left corner blue
jagged.[9].[9] <- R // Assign the right corner red

let mutable currentPlayer = B
let mutable currentGameStatus = InProgress
let mutable redPieces = 14 // number of red pieces
let mutable bluePieces = 14 // number of blue pieces

let printBoard() = 
    //printfn "    0 1 2 3 4 5 6 7 8 9\n"
    for i in 0 .. (jagged.Length - 1) do
        printf "    %*i   " (jagged.Length - i) i // prints with proper alignment
        //printf "%i   " i
        for j in 0 .. i do
            match jagged.[i].[j] with 
            | E -> printf ". " 
            | _ -> printf "%A " jagged.[i].[j]
            
        printfn ""
    //printfn ""
    printfn "\n        0 1 2 3 4 5 6 7 8 9\n"
let other() = // if current player is Red, return blue. Return golden and empty if same state
    match currentPlayer with
    | R -> B
    | B-> R
    | G -> G
    | E -> E

let checkGameStatus() = //check the current state of the game  and update currentGameStatus
    //TODO: Write method
    ()

let isMoveValid x y : bool = //checks whether the current move is valid or not
    
    match (x,y) with 
    | (x,y) when (x < 0 || x > 9 || y < 0 || y > x) -> false // out of range
    | (0,0) -> false // Golden Cog
    | (9,0) -> false // Blue Cog
    | (9,9) -> false // Red Cog
    | _ -> true
    
let rec makeMove() =

    printfn "Current Player: %A" currentPlayer
    printfn "Enter X Coordinate (0-9): "
    let xCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
    printfn "Enter Y Coordinate (0-%i): " xCoord
    let yCoord = Int32.Parse (Console.ReadLine())

    //check if move is valid and then assign

    match isMoveValid xCoord yCoord with
    | true -> 
        jagged.[xCoord].[yCoord] <- currentPlayer // assign current player to the coordindates in jagged
        match currentPlayer with //reduce the number of red and blue pieces left as more are added the board
        | R -> redPieces <- (redPieces-1)
        | B -> bluePieces <- (bluePieces-1)
        | _ -> ()

        printfn "Red: %i Blue: %i\n" redPieces bluePieces


        //Console.Clear()
        printBoard()
        checkGameStatus() // checks the current game status
        currentPlayer <- other() // swap players
        match currentGameStatus with 
        | InProgress -> makeMove()
        | _ -> ()
    | false -> 
        printfn "Invalid Move. Please Try again\n"
        makeMove()


let main = 

    printBoard()
    makeMove()

    //jagged |> Array.sumBy Array.length |> printfn "LENGTH = %i" // to print length of a jagged array
    

main

Console.ReadKey() |> ignore