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

    let getLegalJumps board player =
        []

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

    let isSquareOccupiedByPlayer square player =
        match square with
        | (piece, _) -> match piece with
                        | None -> false
                        | Some (squarePlayer, _) ->
                            match squarePlayer, player with
                            | (Red, Red) -> true
                            | (Black, Black) -> true
                            | _ -> false
    
    let getRedMoves board square =
        //check diagonal southwest and southeast
        let _, location = square
        let (locRow, locCol) = location
        //on the second row this fails because the col is on the edge
        let possibleMoves = [(locRow + 1, locCol - 1); (locRow + 1, locCol + 1)]
        let validLocations = possibleMoves |> List.filter (fun move -> not (isLocationOccupied (board, move)))
        validLocations |> List.map (fun newLoc -> (location, newLoc))

    let getBlackMoves board square =
        //check diagonal southwest and southeast
        let _, location = square
        let (locRow, locCol) = location
        let possibleMoves = [(locRow - 1, locCol - 1); (locRow - 1, locCol + 1)]
        let validLocations = possibleMoves |> List.filter (fun move -> isLocationOccupied (board, move))
        validLocations |> List.map (fun newLoc -> (location, newLoc))


    let hasMove board player square =
        match player with
        | Red -> getRedMoves board square
        | Black -> getBlackMoves board square

    let getLegalNonJumps board player =
        //process the board and find non jump moves for current player
        let x = board
                    |> List.concat
                    |> List.filter (fun square -> isSquareOccupiedByPlayer square player)
                    |> List.map (hasMove board player)
        x

    (*let getLegalMoves (board:Board, player:Player) : Move list =
        let jumps = getLegalJumps(board,player)
        match jumps |> List.length with
        | x when x > 0 -> jumps
        | x when x = 0 -> getLegalNonJumps(board, player)
        | _ -> []

    let getNextMove (board:Board, player:Player) : string =
        //first get a list of legal moves
        //select the first legal move as a naieve approach
        let legalMoves =   getLegalMoves(board, player)
        "E"*)