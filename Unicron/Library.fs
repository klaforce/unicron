namespace Unicron

module Checkers =
    type Player = Red | Black
    type Rank = Soldier | King
    type Piece = Player * Rank
    type Coord = { Row:int; Column:int}
    type Board = Piece option list list


    let getBoardState boardStateCharacter : Piece option =
        match boardStateCharacter with 
        | '.' -> None
        | 'r' -> Some (Red, Soldier)
        | 'b' -> Some (Black, Soldier)
        | 'R' -> Some (Red, King)
        | 'B' -> Some (Black, King)
        | _ -> None

    let parseBoardLine line =
        line |> Seq.toList |> List.map getBoardState
    
    let parseBoard lines : Board =
        lines |> List.map parseBoardLine
