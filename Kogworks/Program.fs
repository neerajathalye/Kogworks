﻿open System
type CellState = 
| R // Red
| B // Blue
| G // Golden
| E // Empty

type NumberOfPlayers = // denotes if the game is 1p or 2p
| One
| Two

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

let mutable currentPlayer = R
let mutable currentGameStatus = InProgress
let mutable redPieces = 14 // number of red pieces
let mutable bluePieces = 14 // number of blue pieces
let mutable currentBlueIndex = 3
let mutable currentRedIndex = 4
let mutable numPlayers = One // number of players - One or Two. Default One

let mutable history = []
let mutable blueMoveList = []
let mutable redMoveList = []

type Node = {
    mutable index : int // represents the 31 pieces - 15 red, 15 blue and 1 golden
    mutable move : Move
    mutable connectedNodes : int list} // represents the list of indices of the connected pieces

let mutable graph = [] // denotes all the 31 pieces along with their coordinates and list of connected nodes

for i in 0 .. 30 do 
    let m = {X = -1; Y = -1; By = E} // Creating a default node to initialize the tree
    let node = {move = m; index = i; connectedNodes = []} // creating an empty connectedNodes list to initialize the tree
    graph <- node::graph

graph <- List.rev graph // reverse the list to get index 0 back on top of the list

// Each item in the list is denoted by an index. Each index corresponds to indices in the tree list
// The golden cog has the index 0
// The first blue cog has the index 1 and subsequent blue cogs have odd indices after 1 as follows : 3, 5, 7 ..
// The first red cog has the index 2 and subsequent red cogs have even indices after 2 as follows : 4, 6, 8 ..

graph <- graph |> List.mapi (fun i v ->  if i = 0 then {move = {X = 0; Y = 0; By = G}; index = 0; connectedNodes = []} else v ) // change index 0 to represent the golden cog
graph <- graph |> List.mapi (fun i v ->  if i = 1 then {move = {X = 9; Y = 0; By = B}; index = 1; connectedNodes = []} else v ) // change index 1 to represent the first blue cog
graph <- graph |> List.mapi (fun i v ->  if i = 2 then {move = {X = 9; Y = 9; By = R}; index = 2; connectedNodes = []} else v ) // change index 2 to represent the first red cog

let printBoard() = 
    
    printfn "\n"

    //for node in tree do
    //    if not (node.move = {X = -1; Y = -1; By = E}) then
    //        printfn "%A\n" node
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
    printfn "Blue: %i Red: %i\n" bluePieces redPieces

let other() = // if current player is Red, return blue. Return golden and empty if same state
    match currentPlayer with
    | R -> B
    | B -> R
    | G -> G
    | E -> E

let isInvalidCoord (x, y) : bool = //checks whether the x,y is a valid coordinate on the board
    (x < 0 || x > 9 || y < 0 || y > x)

let checkTriangle x y player : Move list list = // checks if the coordinates mentioned are part of any triangles and returns the list of triangles

    let mutable triangles = []
    
    let v1 = (x, y-1)
    let v2 = (x-1, y-1)
    let v3 = (x-1, y)
    let v4 = (x, y+1)
    let v5 = (x+1, y+1)
    let v6 = (x+1, y)

    let list = [(v1, v2); (v2, v3); (v3, v4); (v4, v5); (v5, v6); (v6, v1)]

    for (vi, vj) in list do
        if not (isInvalidCoord vi) && not (isInvalidCoord vj) then
            let x1, y1 = vi
            let x2, y2 = vj
            if (jagged.[x1].[y1] = B || jagged.[x1].[y1] = R) && (jagged.[x2].[y2] = B || jagged.[x2].[y2] = R) then
                //printfn "Triangle 1 Valid: %A : %A : %A" (x,y) v1 v2
                let triangle = [{X = x; Y = y; By = jagged.[x].[y]} ; {X = x1; Y = y1; By = jagged.[x1].[y1]} ; {X = x2; Y = y2; By = jagged.[x2].[y2]}]
                triangles <- triangle :: triangles   
    triangles

let checkAllTriangles(): Move list list = // returns all triangles on the board

    let mutable triangleList = []
    for x in 0 .. 9 do 
        for y in 0 .. x do
            let l = checkTriangle x y jagged.[x].[y]
            if not (l.Length = 0) then
                triangleList <- l @ triangleList
    triangleList
    
let hasInvalidTriangle x y player = //Checks if there is an invalid triangle that consists of 3 red or 3 blue pieces
    let list = checkTriangle x y player
    //printfn "%i %i %A" x y player
    //printfn "LIST: %A" list
    let mutable foundTriangle = false // set false by default
    
    if not list.IsEmpty then
        //printfn "List is not Empty"
        for l in list do // for each triangle found in the main list
            //let mutable count = 0
            ////printfn "%A" l
            //if (l.Item 0).By = player then
            //    count <- count + 1
            //if (l.Item 1).By = player then
            //    count <- count + 1
            //if (l.Item 2).By = player then
            //    count <- count + 1
            if ((l.Item 0).By = player && (l.Item 1).By = player && (l.Item 2).By = player) then // if all the 3 coordinates in the list are of the same colour then set foundTriangle to true 
                foundTriangle <- true
            //if count >= 2 then //return true if more than 2 pieces of the player are already part of a triangle
                //foundTriangle <- true
    //printfn "FOUND TRIANGLE: %b" foundTriangle
    foundTriangle // will return true only if a triangle was found 

let isMoveValid x y player : bool = //checks whether the current move is valid or not
    match (x,y,player) with 
    | (x,y,_) when (x < 0 || x > 9 || y < 0 || y > x) -> false // out of range
    | (0,0,_) -> false // Golden Cog
    | (9,0,_) -> false // Blue Cog
    | (9,9,_) -> false // Red Cog
    | (x,y,_) when List.contains {X = x; Y = y; By = R} history -> false // returns false if move is made on a piece already placed by the Red player
    | (x,y,_) when List.contains {X = x; Y = y; By = B} history -> false // returns false if move is made on a piece already placed by the Blue player
    | (8,0,R) when jagged.[9].[1] = R -> false // if (9,1) is already Red, then return false for (8,0)
    | (8,0,B) when jagged.[9].[1] = B -> false // if (9,1) is already Blue, then return false for (8,0)
    | (9,1,R) when jagged.[8].[0] = R -> false // if (8,0) is already Red, then return false for (9,1)
    | (9,1,B) when jagged.[8].[0] = B -> false // if (8,0) is already Blue, then return false for (9,1)
    | (8,8,R) when jagged.[9].[8] = R -> false // if (9,8) is already Red, then return false for (8,8)
    | (8,8,B) when jagged.[9].[8] = B -> false // if (9,8) is already Blue, then return false for (8,8)
    | (9,8,R) when jagged.[8].[8] = R -> false // if (8,8) is already Red, then return false for (9,8)
    | (9,8,B) when jagged.[8].[8] = B -> false // if (8,8) is already Blue, then return false for (9,8)
    | (1,0,R) when jagged.[1].[1] = R -> false // if (1,1) is already Red, then return false for (1,0)
    | (1,0,B) when jagged.[1].[1] = B -> false // if (1,1) is already Blue, then return false for (1,0)
    | (1,1,R) when jagged.[1].[0] = R -> false // if (1,0) is already Red, then return false for (1,1)
    | (1,1,B) when jagged.[1].[0] = B -> false // if (1,0) is already Blue, then return false for (1,1)
    | _ -> true

let rec deleteItemFromList item list = //deletes specified item from the list
    
    match list with
    | head :: tail when head = item -> tail
    | head :: tail -> head :: deleteItemFromList item tail
    | _ -> []

let rec last = function // returns last element of a list
| hd :: [] -> hd
| hd :: tl -> last tl
| _ -> failwith "Empty list."

let getConnectedCogs x y = // finds all the cogs that are adjacent to the cog at (x,y)
    let mutable connectedCogsList = []
    let coordinates = [(x, y-1); (x-1, y-1); (x-1, y); (x, y+1); (x+1, y+1); (x+1, y)]
    for c in coordinates do // returns records of move for all the adjacent cogs
        if not (isInvalidCoord c) then
            let x1,y1 = c
            if (jagged.[x1].[y1] <> E) then
                connectedCogsList <- {X = x1; Y = y1; By = jagged.[x1].[y1]} :: connectedCogsList 

    connectedCogsList // contains all the adjacent cogs (record)

let convertMoveToIndex connectedCogsList = // converts the cog (record) to its equivalent index in the tree
    let mutable indexList = []
    for cog in connectedCogsList do 
        for node in graph do
            if cog = node.move then
                indexList <- node.index :: indexList
    indexList

let findTriangleOnChain ch player = // checks if any cog on the chain is part of a triangle
    
    let mutable triangleFound = false
    let chains = ch |> List.distinct |> List.rev |> List.map List.rev // gets distinct items, reverses the list and reverses all items in the list
    for chain in chains do // for each chain in the list of chains  
        for index in chain do // for each index (node) on the chain
            let x = graph.[index].move.X
            let y = graph.[index].move.Y
            let by = graph.[index].move.By  

            if not (checkTriangle x y by).IsEmpty then // if the list of triangles is not empty, then return true meaning a triangle was found
                triangleFound <- true // triangle was found
            
    triangleFound // triangle was not found.. so return false

let rec traverse index visited oldChains player = // traverse the player's paths   
    
    let mutable chains = [] // keeps track of all the blue paths
    match index with 
    | 0 -> // if index is 0, the golden cog has been reached; No triangle on chain
        let newVisited = 0 :: visited
        chains <- newVisited :: oldChains
        let triangleFound = findTriangleOnChain chains player
        match triangleFound with
        | true -> currentGameStatus <- InProgress
        | false -> 
            match player with 
            | B -> currentGameStatus <- WonByB //change current game status to indicate victory of B
            | R -> currentGameStatus <- WonByR //change current game status to indicate victory of R
            | _ -> ()
        
        chains // return all chains formed
    | ind ->  // for all other indices, call their children recursively and depth first
        if not (List.contains ind visited) then
            let newVisited = ind :: visited
            chains <- newVisited :: oldChains
            //printfn "INDEX: %i VISITED: %A" ind newVisited
            for i in graph.[ind].connectedNodes do
                chains <- (traverse i newVisited chains player) @ chains
        chains  
    
let rec addPiece() = // adds pieces to the board
    
    printfn "Enter X Coordinate (0-9): "
    let xCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
    printfn "Enter Y Coordinate (0-%i): " xCoord
    let yCoord = Int32.Parse (Console.ReadLine()) // parse string input to int

    //Assign a piece and check if move is valid. If not delete it again
    match isMoveValid xCoord yCoord currentPlayer with
    | true -> //make move
        if not (hasInvalidTriangle xCoord yCoord currentPlayer) then
            jagged.[xCoord].[yCoord] <- currentPlayer // assign current player to the coordindates in jagged
            history <- {X = xCoord; Y = yCoord; By = currentPlayer} :: history // add current move to history list
            let connectedCogList = getConnectedCogs xCoord yCoord // contains the list of the adjacent cogs
            let indexList = convertMoveToIndex connectedCogList // converts the cog record to its equivalent index
            match currentPlayer with 
            | B -> 
                graph <- graph |> List.mapi (fun i v ->  if i = currentBlueIndex then {move = {X = xCoord; Y = yCoord; By = B}; index = currentBlueIndex; connectedNodes = indexList} else v ) // change the current piece in the tree list
                for ind in indexList do // for each item in the index list update their index list to contain this cog's index
                    let move = graph.[ind].move
                    let conList = currentBlueIndex :: graph.[ind].connectedNodes
                    graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list
                currentBlueIndex <- currentBlueIndex + 2 // update the currentBlueIndex
                blueMoveList <- {X = xCoord; Y = yCoord; By = B} :: blueMoveList // add the move to the blue move list
                bluePieces <- (bluePieces-1) //reduce the number of blue pieces left
                let bc = traverse 1 [] [] B // traverses from blue's base and gets all paths starting from node 1
                bc |> List.distinct |> List.rev |> List.map List.rev |> printfn "BLUE CHAINS : %A" // gets distinct items, reverses the list and reverses all items in the list
                
                if findTriangleOnChain bc B then // if the new cog is part of a triangle that connects back to the base then consider it illegal
                    
                    currentBlueIndex <- currentBlueIndex - 2 // restore the currentBlueIndex
                    bluePieces <- (bluePieces+1) //restore the number of blue pieces left

                    jagged.[xCoord].[yCoord] <- E // make the cog empty again
                    history <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} history // remove the move from history list
                    blueMoveList <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} blueMoveList // remove the move from the blue move list

                    graph <- graph |> List.mapi (fun i v ->  if i = currentBlueIndex then {move = {X = -1; Y = -1; By = E}; index = currentBlueIndex; connectedNodes = []} else v ) // restore the current piece in the graph list
                    
                    for ind in indexList do // for each item in the index list restore their index list 
                        let move = graph.[ind].move
                        let conList = deleteItemFromList currentBlueIndex graph.[ind].connectedNodes // delete the current index from the neighbouring cogs list
                        graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list

                    printfn "Invalid Move. Please Try again\n"
                    addPiece()
            | R -> 
                graph <- graph |> List.mapi (fun i v ->  if i = currentRedIndex then {move = {X = xCoord; Y = yCoord; By = R}; index = currentRedIndex; connectedNodes = indexList} else v ) // change the current piece in the graph list
                for ind in indexList do // for each item in the index list update their index list to contain this cog's index
                    let move = graph.[ind].move
                    let conList = currentRedIndex :: graph.[ind].connectedNodes
                    graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list
                currentRedIndex <- currentRedIndex + 2 // update the currentRedIndex
                redMoveList <- {X = xCoord; Y = yCoord; By = R} :: redMoveList // add the move to the red move list
                redPieces <- (redPieces-1) //reduce the number of red pieces left
                let rc = traverse 2 [] [] R // traverses from red's base and gets all paths starting from node 2
                rc |> List.distinct |> List.rev |> List.map List.rev |> printfn "RED CHAINS : %A" // gets distinct items, reverses the list and reverses all items in the list
                if findTriangleOnChain rc R then // if the new cog is part of a triangle that connects back to the base then consider it illegal
                    
                    jagged.[xCoord].[yCoord] <- E // make the cog empty again
                    history <- deleteItemFromList {X = xCoord; Y = yCoord; By = R} history // remove the move from history list
                    redMoveList <- deleteItemFromList {X = xCoord; Y = yCoord; By = R} redMoveList // remove the move from the red move list

                    graph <- graph |> List.mapi (fun i v ->  if i = currentRedIndex then {move = {X = xCoord; Y = yCoord; By = E}; index = currentRedIndex; connectedNodes = []} else v ) // restore the current piece in the graph list
                    
                    for ind in indexList do // for each item in the index list restore their index list 
                        let move = graph.[ind].move
                        let conList = deleteItemFromList currentRedIndex graph.[ind].connectedNodes // delete the current index from the neighbouring cogs list
                        graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list
                    
                    currentRedIndex <- currentRedIndex - 2 // restore the currentRedIndex
                    redPieces <- (redPieces + 1) //restore the number of red pieces left

                    printfn "Invalid Move. Please Try again\n"
                    addPiece()
            | _ -> ()
 
        else
            printfn "Invalid Move. Please Try again\n"
            addPiece()
    | false -> 
        printfn "Invalid Move. Please Try again\n"
        addPiece()

let rec movePiece() =  // moves pieces on the board to a new location

    printfn "Enter coordinates of the piece to be moved"
    printfn "Enter X Coordinate (0-9): "
    let xCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
    printfn "Enter Y Coordinate (0-%i): " xCoord
    let yCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
    if not (isMoveValid xCoord yCoord currentPlayer) then
        match currentPlayer with
        | B -> 
            if List.contains {X = xCoord; Y = yCoord; By = B} blueMoveList then   
                printfn "Enter new Coordinates" //get coordinates for the new piece
                printfn "Enter X Coordinate (0-9): "
                let newXCoord = Int32.Parse (Console.ReadLine()) // parse string input to int
                printfn "Enter Y Coordinate (0-%i): " xCoord
                let newYCoord = Int32.Parse (Console.ReadLine()) // parse string input to int

                if not (isMoveValid newXCoord newYCoord B) then // if invalid move

                    printfn "Invalid Move. Please Try again\n"
                    movePiece()
                else
                    jagged.[xCoord].[yCoord] <- E // Make the old peg empty 
                    jagged.[newXCoord].[newYCoord] <- B // assign B to the new coordindates in jagged
                    let mutable orgIndex = -1 // represents the index of the piece that will be moved
                    let mutable orgNodeList = [] // represents the adjacent nodes of the piece that will be moved

                    for node in graph do // gets the original index and node list of the piece to be moved
                        if node.move = {X = xCoord; Y = yCoord; By = B} then
                            orgIndex <- node.index
                            orgNodeList <- node.connectedNodes

                    if hasInvalidTriangle newXCoord newYCoord B then // if (x,y) is part of an invalid triangle then return false and swap the pieces back
                        jagged.[xCoord].[yCoord] <- B // Make the old peg empty again
                        jagged.[newXCoord].[newYCoord] <- E // assign B to the new coordindates in jagged
                        printfn "Invalid Move. Please Try again\n"
                        movePiece()
                    else
                        history <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} history // remove old move from history list
                        blueMoveList <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} blueMoveList // remove old move from the blue move list
                        history <- {X = newXCoord; Y = newYCoord; By = B} :: history // add current move to history list
                        blueMoveList <- {X = newXCoord; Y = newYCoord; By = B} :: blueMoveList // add the move to the blue move list

                        let connectedCogList = getConnectedCogs newXCoord newYCoord // contains the list of the adjacent cogs for the new position
                        let indexList = convertMoveToIndex connectedCogList // converts the cog record to its equivalent index

                        graph <- graph |> List.mapi (fun i v ->  if i = orgIndex then {move = {X = newXCoord; Y = newYCoord; By = B}; index = orgIndex; connectedNodes = indexList} else v ) // change the current piece in the tree list
                    
                        for ind in orgNodeList do // for each item in the original node list update their index list to remove the original cog's index
                            let move = graph.[ind].move
                            let conList = deleteItemFromList orgIndex graph.[ind].connectedNodes // delete the orgIndex from all of its connected nodes
                            graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list

                        for ind in indexList do // for each item in the index list update their index list to contain this cog's index
                            let move = graph.[ind].move
                            let conList = orgIndex :: graph.[ind].connectedNodes
                            graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list
                    
                        let bc = traverse 1 [] [] B // traverses from blue's base and gets all paths starting from node 1
                        bc |> List.distinct |> List.rev |> List.map List.rev |> printfn "BLUE CHAINS : %A" // gets distinct items, reverses the list and reverses all items in the list   

                        if findTriangleOnChain bc B then // if the new cog is part of a triangle that connects back to the base then consider it illegal

                            jagged.[newXCoord].[newYCoord] <- E // make the new cog empty again
                            jagged.[xCoord].[yCoord] <- B // make the old cog blue again
                            history <- deleteItemFromList {X = newXCoord; Y = newYCoord; By = B} history // remove the new move from history list
                            blueMoveList <- deleteItemFromList {X = newXCoord; Y = newYCoord; By = B} blueMoveList // remove the new move from the blue move list
                            history <- {X = xCoord; Y = yCoord; By = B} :: history // add the original move back to history list
                            blueMoveList <- {X = xCoord; Y = yCoord; By = B} :: blueMoveList // add the original move back to the blue move list

                            graph <- graph |> List.mapi (fun i v ->  if i = orgIndex then {move = {X = xCoord; Y = yCoord; By = B}; index = orgIndex; connectedNodes = orgNodeList} else v ) // restore the old piece in the graph list
                        
                            for ind in indexList do // for each item in the index list restore their index list 
                                let move = graph.[ind].move
                                let connectedCogList = getConnectedCogs move.X move.Y // contains the list of the adjacent cogs for the new position
                                let indList = convertMoveToIndex connectedCogList // converts the cog record to its equivalent index
                                graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = indList} else v ) // change the current piece in the tree list

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

                if not (isMoveValid newXCoord newYCoord R) then
                    printfn "Invalid Move. Please Try again\n"
                    movePiece()
                else 
                    jagged.[xCoord].[yCoord] <- E // Make the old peg empty 
                    jagged.[newXCoord].[newYCoord] <- R // assign R to the new coordindates in jagged
                    let mutable orgIndex = -1 // represents the index of the piece that will be moved
                    let mutable orgNodeList = [] // represents the adjacent nodes of the piece that will be moved

                    for node in graph do // gets the original index of the piece to be moved
                        if node.move = {X = xCoord; Y = yCoord; By = R} then
                            orgIndex <- node.index
                            orgNodeList <- node.connectedNodes

                    if hasInvalidTriangle newXCoord newYCoord R then // if (x,y) is part of an invalid triangle then return false and swap the pieces back
                        jagged.[xCoord].[yCoord] <- R // Make the old peg empty again
                        jagged.[newXCoord].[newYCoord] <- E // assign B to the new coordindates in jagged
                        printfn "Invalid Move. Please Try again\n"
                        movePiece()
                    else
                        history <- deleteItemFromList {X = xCoord; Y = yCoord; By = R} history // remove old move from history list
                        redMoveList <- deleteItemFromList {X = xCoord; Y = yCoord; By = R} redMoveList // remove old move from the red move list
                        history <- {X = newXCoord; Y = newYCoord; By = R} :: history // add current move to history list
                        redMoveList <- {X = newXCoord; Y = newYCoord; By = R} :: redMoveList // add the move to the red move list

                        let connectedCogList = getConnectedCogs newXCoord newYCoord // contains the list of the adjacent cogs for the new position
                        let indexList = convertMoveToIndex connectedCogList // converts the cog record to its equivalent index

                        graph <- graph |> List.mapi (fun i v ->  if i = orgIndex then {move = {X = newXCoord; Y = newYCoord; By = R}; index = orgIndex; connectedNodes = indexList} else v ) // change the current piece in the tree list
                    
                        for ind in orgNodeList do // for each item in the original node list update their index list to remove the original cog's index
                            let move = graph.[ind].move
                            let conList = deleteItemFromList orgIndex graph.[ind].connectedNodes // delete the orgIndex from all of its connected nodes
                            graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list

                        for ind in indexList do // for each item in the index list update their index list to contain this cog's index
                            let move = graph.[ind].move
                            let conList = orgIndex :: graph.[ind].connectedNodes
                            graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list

                        let rc = traverse 2 [] [] R // traverses from red's base and gets all paths starting from node 2
                        rc |> List.distinct |> List.rev |> List.map List.rev |> printfn "RED CHAINS : %A" // gets distinct items, reverses the list and reverses all items in the list
                    
                        if findTriangleOnChain rc R then // if the new cog is part of a triangle that connects back to the base then consider it illegal

                            jagged.[newXCoord].[newYCoord] <- E // make the new cog empty again
                            jagged.[xCoord].[yCoord] <- R // make the old cog red again
                            history <- deleteItemFromList {X = newXCoord; Y = newYCoord; By = R} history // remove the new move from history list
                            redMoveList <- deleteItemFromList {X = newXCoord; Y = newYCoord; By = R} redMoveList // remove the new move from the red move list
                            history <- {X = xCoord; Y = yCoord; By = R} :: history // add the original move back to history list
                            redMoveList <- {X = xCoord; Y = yCoord; By = R} :: redMoveList // add the original move back to the red move list

                            graph <- graph |> List.mapi (fun i v ->  if i = orgIndex then {move = {X = xCoord; Y = yCoord; By = R}; index = orgIndex; connectedNodes = orgNodeList} else v ) // restore the old piece in the graph list
                        
                            for ind in indexList do // for each item in the index list restore their index list 
                                let move = graph.[ind].move
                                let connectedCogList = getConnectedCogs move.X move.Y // contains the list of the adjacent cogs for the new position
                                let indList = convertMoveToIndex connectedCogList // converts the cog record to its equivalent index
                                graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = indList} else v ) // change the current piece in the tree list

                            printfn "Invalid Move. Please Try again\n"
                            movePiece()  
            else
                printfn "Illegal Move. Try again"
                movePiece()
        | _ -> ()
    else
        printfn "Illegal Move. Try again"
        movePiece()
    
let chooseRandomValidMove (validMoves:(int*int) list) = // returns a random valid move
    let rand = new System.Random()
    //let xCoord, yCoord = validMoves.[rand.Next(validMoves.Length)] // select the a random move in the valid moves list and assign that to the computer
    let xCoord, yCoord = last validMoves // select the a random move in the valid moves list and assign that to the computer
    xCoord, yCoord

let addComputerPiece (bc:int list list) = 

    let mutable longestChain = bc.[0] // get the longest chain from all the chains formed 
    for chain in bc do // gets the longest chain from all the chains
        if chain.Length > longestChain.Length then
            longestChain <- chain

    printfn "Longest Chain: %A" longestChain 

    let lastKogOnChain = longestChain.Head // Gets the last kog on the chain ( since the list is in reverse)

    let x = graph.[lastKogOnChain].move.X // x coordinate of the last kog on the chain
    let y = graph.[lastKogOnChain].move.Y // x coordinate of the last kog on the chain

    let mutable validMoves = []

    let coordinates = [(x, y-1); (x-1, y-1); (x-1, y); (x, y+1); (x+1, y+1); (x+1, y)] // list of adjacent coordinates

    for c in coordinates do // create a list of valid moves from the kogs that are adjacent to (x,y)
        if not (isInvalidCoord c) then
            validMoves <- c :: validMoves

    for (x,y) in validMoves do //delete the coordinates that are not empty
        if jagged.[x].[y] <> E then 
            validMoves <- deleteItemFromList (x,y) validMoves

    for (x,y) in validMoves do //delete the invalid moves from the list
        if not (isMoveValid x y B) then 
            validMoves <- deleteItemFromList (x,y) validMoves
    
    let rec performValidMove (validMovesList:(int*int) list) = 

        if validMovesList.Length = 0 then // if there are no valid moves, check if there is a triangle
            if findTriangleOnChain bc B then // if triangle found then game over
                currentGameStatus <- WonByR
            else // if there is no triangle then repeatedly choose a random x and y and place a piece there till a valid move is found  
                let mutable emptyCoordinatesList = [] // represents the list of empty coordinates on the board

                for i in 0 .. 9 do
                    for j in 0 .. i do
                        if jagged.[i].[j] = E then
                            emptyCoordinatesList <- (i,j) :: emptyCoordinatesList 

                let rec chooseRandomEmptyCoordinate (list:(int32*int32) list) = 
                    let rand = new System.Random()
                    let xCoord, yCoord = list.[rand.Next(list.Length)] // randomly choose an x and y coordinate from the list

                    if isMoveValid xCoord yCoord B then // check if the selected move is valid
                        if not (hasInvalidTriangle xCoord yCoord B) then
                            jagged.[xCoord].[yCoord] <- B // Assign B to the peg 
                            history <- {X = xCoord; Y = yCoord; By = B} :: history // add current move to history list
                            blueMoveList <- {X = xCoord; Y = yCoord; By = B} :: blueMoveList // add current move to blueMoveList  
        
                            let connectedCogList = getConnectedCogs xCoord yCoord // contains the list of the adjacent cogs
                            let indexList = convertMoveToIndex connectedCogList // converts the cog record to its equivalent index

                            graph <- graph |> List.mapi (fun i v ->  if i = currentBlueIndex then {move = {X = xCoord; Y = yCoord; By = B}; index = currentBlueIndex; connectedNodes = indexList} else v ) // change the current piece in the tree list
                            for ind in indexList do // for each item in the index list update their index list to contain this cog's index
                                let move = graph.[ind].move
                                let conList = currentBlueIndex :: graph.[ind].connectedNodes
                                graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list
        
                            currentBlueIndex <- currentBlueIndex + 2 // update the currentBlueIndex
                            bluePieces <- (bluePieces-1) //reduce the number of blue pieces left

                            let bc = traverse 1 [] [] B // traverses from blue's base and gets all paths starting from node 1
                    
                            if findTriangleOnChain bc B then // if the new cog is part of a triangle that connects back to the base then consider it illegal
                        
                                currentBlueIndex <- currentBlueIndex - 2 // restore the currentBlueIndex
                                bluePieces <- (bluePieces+1) //restore the number of blue pieces left

                                jagged.[xCoord].[yCoord] <- E // make the cog empty again
                                history <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} history // remove the move from history list
                                blueMoveList <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} blueMoveList // remove the move from the blue move list

                                graph <- graph |> List.mapi (fun i v ->  if i = currentBlueIndex then {move = {X = -1; Y = -1; By = E}; index = currentBlueIndex; connectedNodes = []} else v ) // restore the current piece in the graph list
                        
                                for ind in indexList do // for each item in the index list restore their index list 
                                    let move = graph.[ind].move
                                    let conList = deleteItemFromList currentBlueIndex graph.[ind].connectedNodes // delete the current index from the neighbouring cogs list
                                    graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list
                            
                                let newList = deleteItemFromList (xCoord, yCoord) list
                                chooseRandomEmptyCoordinate newList // if a triangle if found, all changes are reverted and perform valid move is called again
                        else
                            let newList = deleteItemFromList (xCoord, yCoord) list
                            chooseRandomEmptyCoordinate newList // if a triangle if found, all changes are reverted and perform valid move is called again
                    else
                        let newList = deleteItemFromList (xCoord, yCoord) list
                        chooseRandomEmptyCoordinate newList // if a triangle if found, all changes are reverted and perform valid move is called again
    
                chooseRandomEmptyCoordinate emptyCoordinatesList

        else
            let mutable xCoord, yCoord = chooseRandomValidMove validMovesList // select the a random move in the valid moves list and assign that to the computer
            if isMoveValid xCoord yCoord B then // check if the selected move is valid
                if not (hasInvalidTriangle xCoord yCoord B) then
                    jagged.[xCoord].[yCoord] <- B // Assign B to the peg 
                    history <- {X = xCoord; Y = yCoord; By = B} :: history // add current move to history list
                    blueMoveList <- {X = xCoord; Y = yCoord; By = B} :: blueMoveList // add current move to blueMoveList  
        
                    let connectedCogList = getConnectedCogs xCoord yCoord // contains the list of the adjacent cogs
                    let indexList = convertMoveToIndex connectedCogList // converts the cog record to its equivalent index

                    graph <- graph |> List.mapi (fun i v ->  if i = currentBlueIndex then {move = {X = xCoord; Y = yCoord; By = B}; index = currentBlueIndex; connectedNodes = indexList} else v ) // change the current piece in the tree list
                    for ind in indexList do // for each item in the index list update their index list to contain this cog's index
                        let move = graph.[ind].move
                        let conList = currentBlueIndex :: graph.[ind].connectedNodes
                        graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list
        
                    currentBlueIndex <- currentBlueIndex + 2 // update the currentBlueIndex
                    bluePieces <- (bluePieces-1) //reduce the number of blue pieces left

                    let bc = traverse 1 [] [] B // traverses from blue's base and gets all paths starting from node 1
            
                    if findTriangleOnChain bc B then // if the new cog is part of a triangle that connects back to the base then consider it illegal
                
                        currentBlueIndex <- currentBlueIndex - 2 // restore the currentBlueIndex
                        bluePieces <- (bluePieces+1) //restore the number of blue pieces left

                        jagged.[xCoord].[yCoord] <- E // make the cog empty again
                        history <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} history // remove the move from history list
                        blueMoveList <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} blueMoveList // remove the move from the blue move list

                        graph <- graph |> List.mapi (fun i v ->  if i = currentBlueIndex then {move = {X = -1; Y = -1; By = E}; index = currentBlueIndex; connectedNodes = []} else v ) // restore the current piece in the graph list
                
                        for ind in indexList do // for each item in the index list restore their index list 
                            let move = graph.[ind].move
                            let conList = deleteItemFromList currentBlueIndex graph.[ind].connectedNodes // delete the current index from the neighbouring cogs list
                            graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list
                    
                        let newValidMoves = deleteItemFromList (xCoord, yCoord) validMovesList
                        performValidMove newValidMoves // if a triangle if found, all changes are reverted and perform valid move is called again
                else
                    let newValidMoves = deleteItemFromList (xCoord, yCoord) validMovesList
                    performValidMove newValidMoves // if a triangle if found, all changes are reverted and perform valid move is called again
            else
                let newValidMoves = deleteItemFromList (xCoord, yCoord) validMovesList
                performValidMove newValidMoves // if a triangle if found, all changes are reverted and perform valid move is called again
    
    performValidMove validMoves // repeatedly call this function till a valid move is made

let moveComputerPiece (bc:int list list) = 

    let mutable longestChain = bc.[0] // get the longest chain from all the chains formed

    for item in longestChain do
        if item = 0 || item = 1 || item % 2 = 0 then
            longestChain <- deleteItemFromList item longestChain

    let rec findKogToBeMoved (chain: int list) = // recursively finds a kog that can be moved that does not leave a triangle after being moved
        
        let mutable xCoord, yCoord = -1, -1 // initialize a mutable xCoord and yCoord to represent the kog to be moved
        let mutable currentIndex = -1

        if chain.Length = 0 then
            currentGameStatus <- WonByR
        else
            for index in chain do // find value for xCoord and yCoord that is part of a triangle
                if not (index = 0 || index = 1 || index % 2 = 0) then // make sure the index is not any of the bases and is not a red kog (even number)
                    let x = graph.[index].move.X
                    let y = graph.[index].move.Y
                    let player = graph.[index].move.By
                    let triangle = checkTriangle x y player // get all the triangles the kog at index is involved in
                    if not triangle.IsEmpty then // if there is a triangle, then choose x,y to be the kog that is moved
                        xCoord <- x
                        yCoord <- y
                        currentIndex <- index

            let connectedCogList = getConnectedCogs xCoord yCoord // contains the list of the adjacent cogs
            let indexList = convertMoveToIndex connectedCogList // converts the cog record to its equivalent index

            let mutable orgNodeList = [] // represents the adjacent nodes of the piece that will be moved
        
            for node in graph do // gets the original node list of the piece to be moved
                if node.move = {X = xCoord; Y = yCoord; By = B} then
                    orgNodeList <- node.connectedNodes

            jagged.[xCoord].[yCoord] <- E // make the kog empty
            bluePieces <- bluePieces + 1 // increment the counter again

            history <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} history // remove the move from history list
            blueMoveList <- deleteItemFromList {X = xCoord; Y = yCoord; By = B} blueMoveList // remove the move from the blue move list

            graph <- graph |> List.mapi (fun i v ->  if i = currentIndex then {move = {X = -1; Y = -1; By = E}; index = currentIndex; connectedNodes = []} else v ) // make the current piece empty in the graph list
        
            for ind in indexList do // for each item in the index list restore their index list 
                let move = graph.[ind].move
                let conList = deleteItemFromList currentIndex graph.[ind].connectedNodes // delete the current index from the neighbouring cogs list
                graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list

            let bc = traverse 1 [] [] B // traverses from blue's base and gets all paths starting from node 1
            if findTriangleOnChain bc B then // if there is still a triangle on the board then revert all the changes made and call the function again but without the kog that was used
                let newChain = deleteItemFromList currentIndex chain // remove currentIndex and call the function again

                //revert all the changes made
                jagged.[xCoord].[yCoord] <- B // make the kog blue again
                bluePieces <- bluePieces - 1 // decrease the counter again

                history <- {X = xCoord; Y = yCoord; By = B} :: history // add the move to the history list again
                blueMoveList <- {X = xCoord; Y = yCoord; By = B} :: blueMoveList // add the move to the blue move list again

                graph <- graph |> List.mapi (fun i v ->  if i = currentIndex then {move = {X = xCoord; Y = yCoord; By = B}; index = currentIndex; connectedNodes = orgNodeList} else v ) // restore the current piece in the graph list

                for ind in indexList do // for each item in the index list update their index list to contain this cog's index
                    let move = graph.[ind].move
                    let conList = currentIndex :: graph.[ind].connectedNodes
                    graph <- graph |> List.mapi (fun i v ->  if i = ind then {move = move; index = ind; connectedNodes = conList} else v ) // change the current piece in the tree list

                findKogToBeMoved newChain
            else // if there is no triangle after removing the piece then move it to another location
                addComputerPiece bc   
        
    findKogToBeMoved longestChain // call this repeatedly till a suitable kog is found that can be moved

let makeComputerMove() = // makes the move for the blue player which is the computer
    
    let bc = traverse 1 [] [] B // traverses from blue's base and gets all paths starting from node 1
    
    if findTriangleOnChain bc B then // if there's a triangle on the chain then move piece, else add piece
        moveComputerPiece bc

    else // no chain, hence add a piece
        addComputerPiece bc

let rec makeMove() = // numPlayers = 1 or 2 meaning 1 player or 2 player

    Console.Clear()
    printBoard()

    match currentGameStatus with 
    | WonByB -> 
        Console.Clear()
        printBoard()
        printfn "GAME OVER!! BLUE WINS!!" //else quit
    | WonByR -> 
        Console.Clear()
        printBoard()
        printfn "GAME OVER!! RED WINS!!" //else quit
    | _ -> 
        let mutable baseBlocked = false

        currentPlayer <- other() // swap players

        if numPlayers = One  && currentPlayer = B then
            makeComputerMove() // makes the move for the blue player which is the computer
            makeMove()
        elif numPlayers = Two || (numPlayers = One && currentPlayer = R) then // do the following only if 2 player game or if the human(R) is playing
            printfn "\nCurrent Player: %A" currentPlayer
    
            if currentPlayer = B then
                let bc = traverse 1 [] [] B // traverses from blue's base and gets all paths starting from node 1
                bc |> List.distinct |> List.rev |> List.map List.rev |> printfn "BLUE CHAINS : %A" // gets distinct items, reverses the list and reverses all items in the list
                if findTriangleOnChain bc B then
                    printfn "Your base is blocked! Please remove the block "
                    baseBlocked <- true
                else 
                    printfn "1. Add a piece"
            elif currentPlayer = R then
                let rc = traverse 2 [] [] R // traverses from red's base and gets all paths starting from node 2
                rc |> List.distinct |> List.rev |> List.map List.rev |> printfn "RED CHAINS : %A" // gets distinct items, reverses the list and reverses all items in the list
                if findTriangleOnChain rc R then
                    printfn "Your base is blocked! Please remove the block "
                    baseBlocked <- true
                else 
                    printfn "1. Add a piece"
        
            match currentPlayer with
            | B ->
                if not (blueMoveList.Length = 0) && not baseBlocked then 
                    printfn "2. Move a piece" // shows up only if a move has been made by blue
            | R ->
                if not (redMoveList.Length = 0) && not baseBlocked then
                    printfn "2. Move a piece" // shows up only if a move has been made by red
            | _ -> ()
    
            match baseBlocked with
            | true -> movePiece()
            | false -> 
                let choice = Console.ReadLine()
                matchChoice choice

        match currentGameStatus with 
        | InProgress -> makeMove() //if game is still in progress, call makeMove again
        | WonByB -> 
            Console.Clear()
            printBoard()
            printfn "GAME OVER!! BLUE WINS!!" //else quit
        | WonByR -> 
            Console.Clear()
            printBoard()
            printfn "GAME OVER!! RED WINS!!" //else quit

and matchChoice choice = 
    
    match choice with
    | "1" -> addPiece()
    | "2" -> movePiece()
    | _ -> 
        printfn "Wrong choice. Try again"
        let choice = Console.ReadLine()
        matchChoice choice

let rec chooseGameType() = 
    printfn "\n\tChoose the type of game..."
    printfn "\t1. 1 player"
    printfn "\t2. 2 player"
    let choice = Console.ReadLine()

    match choice with 
    | "1" -> 
        numPlayers <- One
        makeMove()
    | "2" -> 
        numPlayers <- Two
        makeMove()
    | _ ->
        printfn "Wrong choice. Try again"
        chooseGameType()

let main = 

    chooseGameType()

main

Console.ReadKey() |> ignore