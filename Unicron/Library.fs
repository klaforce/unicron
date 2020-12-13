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

    let convertLocationToString (location:Location) =
        let (row, col) = location
        let colName = match col with 
                        | 1 -> "A"
                        | 2 -> "B"
                        | 3 -> "C"
                        | 4 -> "D"
                        | 5 -> "E"
                        | 6 -> "F"
                        | 7 -> "G"
                        | 8 -> "H"
                        | _ -> ""
        sprintf "%s%i" colName row

    let isLocationOccupied (board:Board, location:Location) =
        let (row, col) = location
        let square = board.[row].[col]
        match square with
            | (piece, _) -> match piece with
                            | None -> false
                            | Some(_) -> true

    let isSquareOccupiedByPlayer player square =
        match square with
        | (piece, _) -> match piece with
                        | None -> false
                        | Some (squarePlayer, _) ->
                            match squarePlayer, player with
                            | (Red, Red) -> true
                            | (Black, Black) -> true
                            | _ -> false
    
    let isMoveInRange loc =
        match loc with
        | (x,y) when x < 0 || x > 7 || y > 7 || y < 0 -> false
        | _ -> true

    let isMovePossibleForPlayer player loc =
        match loc with
        | (x,y) when x < 0 || x > 7 || y > 7 || y < 0 -> false
        | _ -> true

    let generatePossibleMoves (board:Board, square:Square): Move list =
        let piece, location = square
        let player = match piece with
                     | None -> None
                     | Some(player, _) -> Some player
        
        let (locRow, locCol) = location
        
        //generate all 4 moves, 
        let possibleLocations = [(locRow + 1, locCol - 1); (locRow + 1, locCol + 1); (locRow - 1, locCol - 1); (locRow - 1, locCol + 1);]
        
        possibleLocations 
            |> List.filter isMoveInRange 
            |> List.filter (isMovePossibleForPlayer player) 
            |> List.filter (fun move -> not (isLocationOccupied (board, move)))
            |> List.map (fun move -> (location, move))

    let getLegalMoves (board:Board, player:Player) : Move list=
        //process the board and find non jump moves for current player
        let x = board
                    |> List.concat
                    |> List.filter (isSquareOccupiedByPlayer player)
                    |> List.collect (fun square -> generatePossibleMoves(board, square))
        x
(*
    let getNextMove (board:Board, player:Player) : string =
        //first get a list of legal moves
        //select the first legal move as a naieve approach
        let legalMoves =   getLegalMoves(board, player)
        "E"*)