open System
open System.Collections.Generic

type CellState = 
| R // Red
| B // Blue
| G // Golden
| E // Empty

type GameStatus = 
| InProgress
| WonByB
| WonByR 

type Move = {
    X : int
    Y : int
    By: CellState }

let jagged = [| for a in 1 .. 10 do yield [| for a in 1 .. a do yield E |] |] // create a 10x10 jagged array with increasing number of pegs on each row and assign them empty

jagged.[0].[0] <- G // Assign the top corner golden
jagged.[9].[0] <- B // Assign the left corner blue
jagged.[9].[9] <- R // Assign the right corner red

let mutable currentPlayer = B
let mutable currentGameStatus = InProgress
let mutable redPieces = 14 // number of red pieces
let mutable bluePieces = 14 // number of blue pieces

let history = new List<Move>()
let blueMoveList = new List<Move>()
let redMoveList = new List<Move>()

//let mutable history = 

let printBoard() = 
    //printfn "    0 1 2 3 4 5 6 7 8 9\n"
    printfn ""
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
    printfn "Red: %i Blue: %i\n" redPieces bluePieces

let other() = // if current player is Red, return blue. Return golden and empty if same state
    match currentPlayer with
    | R -> B
    | B-> R
    | G -> G
    | E -> E

let checkGameStatus() = //check the current state of the game  and update currentGameStatus
    //TODO: Write method
    ()

let isMoveValid x y player : bool = //checks whether the current move is valid or not
    match (x,y) with 
    | (x,y) when (x < 0 || x > 9 || y < 0 || y > x) -> false // out of range
    | (0,0) -> false // Golden Cog
    | (9,0) -> false // Blue Cog
    | (9,9) -> false // Red Cog
    | (x,y) when history.Contains {X = x; Y = y; By = R} -> false // returns false if move is made on a piece already placed by the Red player
    | (x,y) when history.Contains {X = x; Y = y; By = B} -> false // returns false if move is made on a piece already placed by the Blue player
    | _ -> true

    
let rec addPiece() =
    printfn "Enter X Coordinate (0-9): "
    let xCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
    printfn "Enter Y Coordinate (0-%i): " xCoord
    let yCoord = Int32.Parse (Console.ReadLine()) // parse string input to int

    //check if move is valid and then assign

    match isMoveValid xCoord yCoord currentPlayer with
    | true -> //make move
        jagged.[xCoord].[yCoord] <- currentPlayer // assign current player to the coordindates in jagged
        history.Add {X = xCoord; Y = yCoord; By = currentPlayer} // add current move to history list
        match currentPlayer with 
        | B -> 
            blueMoveList.Add {X = xCoord; Y = yCoord; By = B} // add the move to the blue move list
            bluePieces <- (bluePieces-1) //reduce the number of blue pieces left
        | R -> 
            redMoveList.Add {X = xCoord; Y = yCoord; By = R} // add the move to the red move list
            redPieces <- (redPieces-1) //reduce the number of red pieces left
        | _ -> ()

        Console.Clear()
        printBoard()
        //printfn "\n%A\n" history 

        currentPlayer <- other() // swap players
        
    | false -> 
        printfn "Invalid Move. Please Try again\n"
        addPiece()

let rec movePiece() = 

    printfn "Enter coordinates of the piece to be moved"
    printfn "Enter X Coordinate (0-9): "
    let xCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
    printfn "Enter Y Coordinate (0-%i): " xCoord
    let yCoord = Int32.Parse (Console.ReadLine()) // parse string input to int

    match currentPlayer with
    | B -> 
        if blueMoveList.Contains {X = xCoord; Y = yCoord; By = B} then
            
            printfn "Enter new Coordinates" //get coordinates for the new piece
            printfn "Enter X Coordinate (0-9): "
            let newXCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
            printfn "Enter Y Coordinate (0-%i): " xCoord
            let newYCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
            match isMoveValid newXCoord newYCoord B with
            | true -> //make move
                history.Remove {X = xCoord; Y = yCoord; By = B} |> ignore // remove old move from history list
                blueMoveList.Remove {X = xCoord; Y = yCoord; By = B} |> ignore // remove old move from the blue move list
                jagged.[xCoord].[yCoord] <- E // Make the old peg empty again
                jagged.[newXCoord].[newYCoord] <- B // assign B to the new coordindates in jagged
                history.Add {X = newXCoord; Y = newYCoord; By = B} // add current move to history list
                blueMoveList.Add {X = newXCoord; Y = newYCoord; By = B} // add the move to the blue move list

                Console.Clear()
                printBoard()
                //printfn "\n%A\n" history 

                currentPlayer <- other() // swap players
                   
            | false -> 
                printfn "Invalid Move. Please Try again\n"
                movePiece()
        else
            printfn "Illegal Move. Try again"
            movePiece()
    | R -> 
        if redMoveList.Contains {X = xCoord; Y = yCoord; By = R} then
           printfn "Enter new Coordinates" //get coordinates for the new piece
           printfn "Enter X Coordinate (0-9): "
           let newXCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
           printfn "Enter Y Coordinate (0-%i): " xCoord
           let newYCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
           match isMoveValid newXCoord newYCoord R with
           | true -> //make move
               history.Remove {X = xCoord; Y = yCoord; By = R} |> ignore // remove old move from history list
               redMoveList.Remove {X = xCoord; Y = yCoord; By = R} |> ignore // remove old move from the red move list
               jagged.[xCoord].[yCoord] <- E // Make the old peg empty again
               jagged.[newXCoord].[newYCoord] <- R // assign B to the new coordindates in jagged
               history.Add {X = newXCoord; Y = newYCoord; By = R} // add current move to history list
               redMoveList.Add {X = newXCoord; Y = newYCoord; By = R} // add the move to the red move list

               Console.Clear()
               printBoard()
               //printfn "\n%A\n" history 

               currentPlayer <- other() // swap players
                  
           | false -> 
               printfn "Invalid Move. Please Try again\n"
               movePiece()
        else
            printfn "Illegal Move. Try again"
            movePiece()
    | _ -> ()


let rec makeMove() = 
    printfn "Current Player: %A" currentPlayer
    printfn "1. Add a piece"

    match currentPlayer with
    | B ->
        if not (blueMoveList.Count = 0) then 
            printfn "2. Move a piece" // shows up only if a move has been made by blue
    | R ->
        if not (redMoveList.Count = 0) then
            printfn "2. Move a piece" // shows up only if a move has been made by red
    | _ -> ()
    let choice = Int32.Parse (Console.ReadLine()) // parse string input to int

    match choice with
    | 1 -> addPiece()
    | 2 -> movePiece()
    | _ -> 
        printfn "Wrong choice. Try again"
        makeMove()

    checkGameStatus() // checks the current game status
    match currentGameStatus with 
    | InProgress -> makeMove() //if game is still in progress, call makeMove again
    | _ -> () //else quit


let main = 

    printBoard()
    makeMove()

    //jagged |> Array.sumBy Array.length |> printfn "LENGTH = %i" // to print length of a jagged array
    

main

Console.ReadKey() |> ignore