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

jagged.[4].[0] <- B
jagged.[5].[0] <- B
jagged.[5].[1] <- R
jagged.[4].[1] <- R
jagged.[5].[2] <- R

jagged.[1].[1] <- R
jagged.[2].[1] <- R
jagged.[2].[2] <- R

let mutable currentPlayer = B
let mutable currentGameStatus = InProgress
let mutable redPieces = 14 // number of red pieces
let mutable bluePieces = 14 // number of blue pieces

let mutable history = []
let mutable blueMoveList = []
let mutable redMoveList = []

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

let isInvalidCoord (x, y) : bool = //checks whether the x,y is a valid coordinate on the board
    (x < 0 || x > 9 || y < 0 || y > x)
    
//let isInvalidCoord(move: Move) : bool = //checks whether the x,y is a valid coordinate on the board
//    (move.X < 0 || move.X > 9 || move.Y < 0 || move.Y > move.X)

let isMoveValid x y player : bool = //checks whether the current move is valid or not
    match (x,y) with 
    | (x,y) when (x < 0 || x > 9 || y < 0 || y > x) -> false // out of range
    | (0,0) -> false // Golden Cog
    | (9,0) -> false // Blue Cog
    | (9,9) -> false // Red Cog
    | (x,y) when List.contains {X = x; Y = y; By = R} history -> false // returns false if move is made on a piece already placed by the Red player
    | (x,y) when List.contains {X = x; Y = y; By = B} history -> false // returns false if move is made on a piece already placed by the Blue player
    | _ -> true

let checkTriangle x y player : Move list list = 

    //let triangleList = new List<Move>()
    //let triangleList = new List<Move>()
    let mutable triangle = []
    
    let v1 = (x, y-1)
    let v2 = (x-1, y-1)
    let v3 = (x-1, y)
    let v4 = (x, y+1)
    let v5 = (x+1, y+1)
    let v6 = (x+1, y)
    
    //let v1 = {X = x; Y = y-1; By = jagged.[x].[y-1]}
    //let v2 = {X = x-1; Y = y-1; By = jagged.[x-1].[y-1]}
    //let v3 = {X = x-1; Y = y; By = jagged.[x-1].[y]}
    //let v4 = {X = x; Y = y+1; By = jagged.[x].[y+1]}
    //let v5 = {X = x+1; Y = y+1; By = jagged.[x+1].[y+1]}
    //let v6 = {X = x+1; Y = y; By = jagged.[x+1].[y]}

    if not (jagged.[x].[y] = E) then
        printfn "\n %i %i %A" x y player
        if not (isInvalidCoord v1) && not (isInvalidCoord v2) then
            let x1, y1 = v1
            let x2, y2 = v2
            if (jagged.[x1].[y1] = B || jagged.[x1].[y1] = R) && (jagged.[x2].[y2] = B || jagged.[x2].[y2] = R) then
                printfn "Triangle 1 Valid: %A : %A : %A" (x,y) v1 v2
                let tuple = [{X = x; Y = y; By = jagged.[x].[y]} ; {X = x1; Y = y1; By = jagged.[x1].[y1]} ; {X = x2; Y = y2; By = jagged.[x2].[y2]}]
                triangle <- tuple :: triangle      
        if not (isInvalidCoord v2) && not (isInvalidCoord v3) then
            let x1, y1 = v2
            let x2, y2 = v3
            if (jagged.[x1].[y1] = B || jagged.[x1].[y1] = R) && (jagged.[x2].[y2] = B || jagged.[x2].[y2] = R) then
                printfn "Triangle 2 Valid: %A : %A : %A" (x,y) v2 v3
                let tuple = [{X = x; Y = y; By = jagged.[x].[y]} ; {X = x1; Y = y1; By = jagged.[x1].[y1]} ; {X = x2; Y = y2; By = jagged.[x2].[y2]}]
                triangle <- tuple :: triangle
        if not (isInvalidCoord v3) && not (isInvalidCoord v4) then
            let x1, y1 = v3
            let x2, y2 = v4
            if (jagged.[x1].[y1] = B || jagged.[x1].[y1] = R) && (jagged.[x2].[y2] = B || jagged.[x2].[y2] = R) then
                printfn "Triangle 3 Valid: %A : %A : %A" (x,y) v3 v4
                let tuple = [{X = x; Y = y; By = jagged.[x].[y]} ; {X = x1; Y = y1; By = jagged.[x1].[y1]} ; {X = x2; Y = y2; By = jagged.[x2].[y2]}]
                triangle <- tuple :: triangle
        if not (isInvalidCoord v4) && not (isInvalidCoord v5) then
            let x1, y1 = v4
            let x2, y2 = v5
            if (jagged.[x1].[y1] = B || jagged.[x1].[y1] = R) && (jagged.[x2].[y2] = B || jagged.[x2].[y2] = R) then
                printfn "Triangle 4 Valid: %A : %A : %A" (x,y) v4 v5
                let tuple = [{X = x; Y = y; By = jagged.[x].[y]} ; {X = x1; Y = y1; By = jagged.[x1].[y1]} ; {X = x2; Y = y2; By = jagged.[x2].[y2]}]
                triangle <- tuple :: triangle
        if not (isInvalidCoord v5) && not (isInvalidCoord v6) then
            let x1, y1 = v5
            let x2, y2 = v6
            if (jagged.[x1].[y1] = B || jagged.[x1].[y1] = R) && (jagged.[x2].[y2] = B || jagged.[x2].[y2] = R) then
                printfn "Triangle 5 Valid: %A : %A : %A" (x,y) v5 v6
                let tuple = [{X = x; Y = y; By = jagged.[x].[y]} ; {X = x1; Y = y1; By = jagged.[x1].[y1]} ; {X = x2; Y = y2; By = jagged.[x2].[y2]}]
                triangle <- tuple :: triangle
        if not (isInvalidCoord v6) && not (isInvalidCoord v1) then
            let x1, y1 = v6
            let x2, y2 = v1
            if (jagged.[x1].[y1] = B || jagged.[x1].[y1] = R) && (jagged.[x2].[y2] = B || jagged.[x2].[y2] = R) then
                printfn "Triangle 6 Valid: %A : %A : %A" (x,y) v6 v1
                let tuple = [{X = x; Y = y; By = jagged.[x].[y]} ; {X = x1; Y = y1; By = jagged.[x1].[y1]} ; {X = x2; Y = y2; By = jagged.[x2].[y2]}]
                triangle <- tuple :: triangle
                //triangleList.Add {X = x; Y = y; By = jagged.[x].[y]}
                //triangleList.Add {X = x1; Y = y1; By = jagged.[x1].[y1]}
                //triangleList.Add {X = x2; Y = y2; By = jagged.[x2].[y2]}
    
    triangle

    //if not (isInvalidCoord v1) && not (isInvalidCoord v2) && (v1.By = B || v1.By = R) && (v2.By = B || v2.By = R)then
    //    printfn "Triangle 1 Valid: %A : %A" v1 v2
    //else 
    //    printfn "Triangle 1 Invalid"

    //if not (isInvalidCoord v2) && not (isInvalidCoord v3) && (v2.By = B || v2.By = R) && (v3.By = B || v3.By = R) then
        
    //    printfn "Triangle 2 Valid: %A : %A" v2 v3
    //else 
    //    printfn "Triangle 2 Invalid"

    //if not (isInvalidCoord v3) && not (isInvalidCoord v4) && (v3.By = B || v3.By = R) && (v4.By = B || v4.By = R) then
        
    //    printfn "Triangle 3 Valid: %A : %A" v3 v4
    //else 
    //    printfn "Triangle 3 Invalid"

    //if not (isInvalidCoord v4) && not (isInvalidCoord v5) && (v4.By = B || v4.By = R) && (v5.By = B || v5.By = R) then
        
    //    printfn "Triangle 4 Valid: %A : %A" v4 v5
    //else 
    //    printfn "Triangle 4 Invalid"

    //if not (isInvalidCoord v5) && not (isInvalidCoord v6) && (v5.By = B || v5.By = R)  && (v6.By = B || v6.By = R) then
        
    //    printfn "Triangle 5 Valid: %A : %A" v5 v6
    //else 
    //    printfn "Triangle 5 Invalid"

    //if not (isInvalidCoord v6) && not (isInvalidCoord v1) && (v6.By = B || v6.By = R) && (v1.By = B || v1.By = R) then
        
    //    printfn "Triangle 6 Valid: %A : %A" v6 v1
    //else 
    //    printfn "Triangle 6 Invalid"

let checkAllTriangles(): Move list list = 

    let mutable triangleList = []
    for x in 0 .. 9 do 
        for y in 0 .. x do
            let l = checkTriangle x y jagged.[x].[y]
            if not (l.Length = 0) then
                triangleList <- l @ triangleList
    triangleList

let rec deleteItemFromList item list = //deletes specified item from the list
    
    match list with
    | head :: tail when head = item -> tail
    | head :: tail -> head :: deleteItemFromList item tail
    | _ -> []

    
let rec addPiece() =
    printfn "Enter X Coordinate (0-9): "
    let xCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
    printfn "Enter Y Coordinate (0-%i): " xCoord
    let yCoord = Int32.Parse (Console.ReadLine()) // parse string input to int

    //check if move is valid and then assign

    match isMoveValid xCoord yCoord currentPlayer with
    | true -> //make move
        jagged.[xCoord].[yCoord] <- currentPlayer // assign current player to the coordindates in jagged
        history <- {X = xCoord; Y = yCoord; By = currentPlayer} :: history // add current move to history list
        match currentPlayer with 
        | B -> 
            blueMoveList <- {X = xCoord; Y = yCoord; By = B} :: blueMoveList // add the move to the blue move list
            bluePieces <- (bluePieces-1) //reduce the number of blue pieces left
        | R -> 
            redMoveList <- {X = xCoord; Y = yCoord; By = R} :: redMoveList // add the move to the red move list
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
        if List.contains {X = xCoord; Y = yCoord; By = B} blueMoveList then   
            printfn "Enter new Coordinates" //get coordinates for the new piece
            printfn "Enter X Coordinate (0-9): "
            let newXCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
            printfn "Enter Y Coordinate (0-%i): " xCoord
            let newYCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
            match isMoveValid newXCoord newYCoord B with
            | true -> //make move
                
                history <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} history // remove old move from history list
                //history.Remove {X = xCoord; Y = yCoord; By = B} 
                blueMoveList <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} blueMoveList // remove old move from the blue move list
                //blueMoveList.Remove {X = xCoord; Y = yCoord; By = B} |> ignore // remove old move from the blue move list
                jagged.[xCoord].[yCoord] <- E // Make the old peg empty again
                jagged.[newXCoord].[newYCoord] <- B // assign B to the new coordindates in jagged
                history <- {X = newXCoord; Y = newYCoord; By = B} :: history // add current move to history list
                blueMoveList <- {X = newXCoord; Y = newYCoord; By = B} :: blueMoveList // add the move to the blue move list

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
        if List.contains {X = xCoord; Y = yCoord; By = R} redMoveList then
           printfn "Enter new Coordinates" //get coordinates for the new piece
           printfn "Enter X Coordinate (0-9): "
           let newXCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
           printfn "Enter Y Coordinate (0-%i): " xCoord
           let newYCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
           match isMoveValid newXCoord newYCoord R with
           | true -> //make move
               history <- deleteItemFromList {X = xCoord; Y = yCoord; By = R} history // remove old move from history list
               //history.Remove {X = xCoord; Y = yCoord; By = R} 
               redMoveList <- deleteItemFromList {X = xCoord; Y = yCoord; By = R} redMoveList // remove old move from the red move list
               //redMoveList.Remove {X = xCoord; Y = yCoord; By = R} |> ignore // remove old move from the red move list
               jagged.[xCoord].[yCoord] <- E // Make the old peg empty again
               jagged.[newXCoord].[newYCoord] <- R // assign B to the new coordindates in jagged
               history <- {X = newXCoord; Y = newYCoord; By = R} :: history // add current move to history list
               redMoveList <- {X = newXCoord; Y = newYCoord; By = R} :: blueMoveList // add the move to the red move list

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
        if not (blueMoveList.Length = 0) then 
            printfn "2. Move a piece" // shows up only if a move has been made by blue
    | R ->
        if not (redMoveList.Length = 0) then
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

    let triangleList = checkAllTriangles() |> List.map List.sort |> List.distinct //get a distinct list of all the triangles on the board


    printfn "Count : %i" triangleList.Length
    for l in triangleList do
        printfn "%A" l

    //printfn "Count : %i" list2.Length
    //printfn "%A" (list2)
    //makeMove()

    //jagged |> Array.sumBy Array.length |> printfn "LENGTH = %i" // to print length of a jagged array
    

main

Console.ReadKey() |> ignore