namespace Unicron

module Checkers =
    type Player = Red | Black
    type Rank = Soldier | King
    type Piece = Player * Rank
    type Move = ((int * int) * (int * int))
    type Location = (int * int)
    type Square = (Piece option * Location)
    type Board = Square list list

    let getBoardState row col boardStateCharacter : Square =
        match boardStateCharacter with 
        | '.' -> (None, (row, col))
        | 'r' -> (Some (Red, Soldier), (row, col))
        | 'b' -> (Some (Black, Soldier), (row, col))
        | 'R' -> (Some (Red, King), (row, col))
        | 'B' -> (Some (Black, King), (row, col))
        | _ -> (None, (row, col))

    let parseBoardLine row line =
        line |> Seq.toList |> List.mapi (getBoardState row)
    
    let parseBoard lines : Board =
        lines |> List.mapi parseBoardLine

    let isLocationInRange loc =
        match loc with
        | (x,y) when x < 0 || x > 7 || y > 7 || y < 0 -> false
        | _ -> true

    let convertLocationToString (location:Location) =
        let (row, col) = location
        let colName = match col with 
                        | 0 -> "A"
                        | 1 -> "B"
                        | 2 -> "C"
                        | 3 -> "D"
                        | 4 -> "E"
                        | 5 -> "F"
                        | 6 -> "G"
                        | 7 -> "H"
                        | _ -> ""
        let rowName = match row with 
                        | 0 -> "8"
                        | 1 -> "7"
                        | 2 -> "6"
                        | 3 -> "5"
                        | 4 -> "4"
                        | 5 -> "3"
                        | 6 -> "2"
                        | 7 -> "1"
                        | _ -> ""
        sprintf "%s%s" colName rowName

    let isLocationOccupied (board:Board, location:Location) =
        let (row, col) = location
        let (piece, _) = board.[row].[col]
        match piece with
            | None -> false
            | Some(_) -> true

    let isLocationOccupiedAndJumpOpen (board:Board, location:Location, jumpLocation:Location, currPiece:Piece option) =
        //first check and see if the jump location is reachable
        match isLocationInRange jumpLocation with
        | false -> false
        | true ->
            let (row, col) = location
            let (jumpRow, jumpCol) = jumpLocation
            let (piece, _) = board.[row].[col]
            let (jumpPiece, _) =  board.[jumpRow].[jumpCol]
            let currPlayer = match currPiece with
                                 | Some (currPlayer, _) -> currPlayer
                                 | None -> Red ///should never get here

            match piece, jumpPiece with
                | Some(occupiedPlayer, _), None ->
                    match currPlayer, occupiedPlayer with
                        | Red, Red -> false //can't jump yourself
                        | Black, Black -> false
                        | Red, Black -> true
                        | Black, Red -> true
                | _, _ -> false

    let isSquareOccupiedByPlayer player square =
        match square with
        | (piece, _) -> match piece with
                        | None -> false
                        | Some (squarePlayer, _) ->
                            match squarePlayer, player with
                            | (Red, Red) -> true
                            | (Black, Black) -> true
                            | _ -> false

    let isLocationPossibleForPiece (piece:Piece option, loc:Location, currLocation:Location) : bool =
        match piece with
        | None -> false
        | Some piece ->
            match piece with
            | (Red, Soldier) -> 
                match loc, currLocation with
                | ((locRow, _), (currRow, _)) when locRow <= currRow -> false //can't go backwards
                | ((locRow, _), (currRow, _)) when locRow > currRow -> true //can't go backwards
                | _ -> false
            | (Black, Soldier) ->
                match loc, currLocation with
                | ((locRow, _), (currRow, _)) when locRow >= currRow -> false //can't go backwards
                | ((locRow, _), (currRow, _)) when locRow < currRow -> true //can't go backwards
                | _ -> false
            | (Red, King) -> true //King can move in all four directions, so never false.
            | (Black, King) -> true //King can move in all four directions, so never false.

    let generatePossibleNonJumpMoves (board:Board, square:Square): Move list =
        let piece, currLocation = square
        let (locRow, locCol) = currLocation
        
        //generate all 4 possible moves and then filter on possibility
        let possibleLocations = [(locRow + 1, locCol - 1); (locRow + 1, locCol + 1); (locRow - 1, locCol - 1); (locRow - 1, locCol + 1);]
        
        possibleLocations 
            |> List.filter isLocationInRange 
            |> List.filter (fun loc -> isLocationPossibleForPiece(piece, loc,currLocation)) 
            |> List.filter (fun move -> not (isLocationOccupied (board, move)))
            |> List.map (fun move -> (currLocation, move))

    let generatePossibleJumpMoves (board:Board, square:Square): Move list =
        let piece, currLocation = square
        let (locRow, locCol) = currLocation
        
        //generate all 4 possible moves and then filter on possibility
        let possibleJumps = [( (locRow + 1, locCol - 1), (locRow + 2, locCol - 2)); 
            ((locRow + 1, locCol + 1), (locRow + 2, locCol + 2));
            ((locRow - 1, locCol - 1), (locRow - 2, locCol - 2));
            ((locRow - 1, locCol + 1), (locRow - 2, locCol + 2))]
        
        possibleJumps 
            |> List.filter (fun jump -> isLocationInRange (fst jump))  
            |> List.filter (fun jump -> isLocationPossibleForPiece(piece, (fst jump), currLocation)) 
            |> List.filter (fun jump -> isLocationOccupiedAndJumpOpen (board, (fst jump), (snd jump), piece))
            |> List.map (fun jump -> (currLocation, (snd jump))) //pick the jump location

    let getLegalMoves (board:Board, player:Player) : Move list=
        //process the board and find non jump moves for current player
        let playerSquares = board
                            |> List.concat
                            |> List.filter (isSquareOccupiedByPlayer player)
                    
        let jumpMoves = playerSquares |> List.collect (fun square -> generatePossibleJumpMoves(board, square))

        match jumpMoves |> List.length with
            | x when x > 0 -> jumpMoves
            | x when x <= 0 -> playerSquares |> List.collect (fun square -> generatePossibleNonJumpMoves(board, square))
            | _ -> []
(*
    let getNextMove (board:Board, player:Player) : string =
        //first get a list of legal moves
        //select the first legal move as a naieve approach
        let legalMoves =   getLegalMoves(board, player)
        "E"*)