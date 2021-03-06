﻿namespace Unicron
open System

module Checkers = 
    (*Type information for the game checkers*)
    type Player =
        | Red
        | Black

    type Rank =
        | Soldier
        | King

    type Piece = Player * Rank
    type Location = (int * int)

    type Move =
        { origin: Location; destination: Location; jump: bool; jumpLocations: Location list }

    type Square = (Piece option * Location)
    type Board = Square list list

    (*Type information for the monte carlo tree search*)
    type GameState =
        { board: Board;
            currPlayer: Player;
            nextPlayer: Player;
            previousState: Option<GameState>;
            lastMove: Option<Move> }

    type MCTSNode =
        { possibleMoves: seq<Move>;
            children: Map<Move, MCTSNode>;
            numRollouts: int;
            gameState: GameState;
            winCounts: Map<Player, int> }

    type GameResult =
        { winner: Player;
            winningMargin: float }

    type CheckerCount =
        { blackCheckers: int;
            redCheckers: int }

    (*Functions that drive the game engine*)
    let getBoardState row col boardStateCharacter: Square =
        match boardStateCharacter with
        | '.' -> (None, (row, col))
        | 'r' -> (Some(Red, Soldier), (row, col))
        | 'b' -> (Some(Black, Soldier), (row, col))
        | 'R' -> (Some(Red, King), (row, col))
        | 'B' -> (Some(Black, King), (row, col))
        | _ -> (None, (row, col))

    let parseBoardLine row line =
        line
        |> Seq.toList
        |> List.mapi (getBoardState row)

    let parseBoard lines: Board = lines |> List.mapi parseBoardLine

    let isLocationInRange loc =
        match loc with
        | (x, y) when x < 0 || x > 7 || y > 7 || y < 0 -> false
        | _ -> true

    let convertLocationToString (location: Location) =
        let (row, col) = location

        let colName =
            match col with
            | 0 -> "A"
            | 1 -> "B"
            | 2 -> "C"
            | 3 -> "D"
            | 4 -> "E"
            | 5 -> "F"
            | 6 -> "G"
            | 7 -> "H"
            | _ -> ""

        let rowName =
            match row with
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

    let isLocationOccupied (board: Board, location: Location) =
        let (row, col) = location
        let (piece, _) = board.[row].[col]

        match piece with
        | None -> false
        | Some (_) -> true

    let isLocationOccupiedAndJumpOpen (board: Board, location: Location, jumpLocation: Location, currPiece: Piece option) =
        //first check and see if the jump location is reachable
        match isLocationInRange jumpLocation with
        | false -> false
        | true ->
            let (row, col) = location
            let (jumpRow, jumpCol) = jumpLocation
            let (piece, _) = board.[row].[col]
            let (jumpPiece, _) = board.[jumpRow].[jumpCol]

            let currPlayer =
                match currPiece with
                | Some (currPlayer, _) -> currPlayer
                | None -> Red ///should never get here

            match piece, jumpPiece with
            | Some (occupiedPlayer, _), None ->
                match currPlayer, occupiedPlayer with
                | Red, Red -> false //can't jump yourself
                | Black, Black -> false
                | Red, Black -> true
                | Black, Red -> true
            | _, _ -> false

    let isSquareOccupiedByPlayer player square =
        match square with
        | (piece, _) ->
            match piece with
            | None -> false
            | Some (squarePlayer, _) ->
                match squarePlayer, player with
                | (Red, Red) -> true
                | (Black, Black) -> true
                | _ -> false

    let isLocationPossibleForPiece (piece: Piece option, loc: Location, currLocation: Location): bool =
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

    let generatePossibleNonJumpMoves (board: Board, square: Square): Move list =
        let piece, currLocation = square
        let (locRow, locCol) = currLocation

        //generate all 4 possible moves and then filter on possibility
        let possibleLocations =
            [ Location(locRow + 1, locCol - 1);
                Location(locRow + 1, locCol + 1);
                Location(locRow - 1, locCol - 1);
                Location(locRow - 1, locCol + 1) ]

        possibleLocations
        |> List.filter isLocationInRange
        |> List.filter (fun loc -> isLocationPossibleForPiece (piece, loc, currLocation))
        |> List.filter (fun move -> not (isLocationOccupied (board, move)))
        |> List.map (fun move ->
            { destination = move;
                origin = currLocation;
                jump = false;
                jumpLocations = [] })

    let generatePossibleJumpMoves (board: Board, square: Square): Move list =
        let piece, currLocation = square
        let (locRow, locCol) = currLocation

        //generate all 4 possible moves and then filter on possibility
        let possibleJumps =
            [ ((locRow + 1, locCol - 1), (locRow + 2, locCol - 2));
                ((locRow + 1, locCol + 1), (locRow + 2, locCol + 2));
                ((locRow - 1, locCol - 1), (locRow - 2, locCol - 2));
                ((locRow - 1, locCol + 1), (locRow - 2, locCol + 2)) ]

        possibleJumps
        |> List.filter (fun jump -> isLocationInRange (fst jump))
        |> List.filter (fun jump -> isLocationPossibleForPiece (piece, (fst jump), currLocation))
        |> List.filter (fun jump -> isLocationOccupiedAndJumpOpen (board, (fst jump), (snd jump), piece))
        |> List.map (fun jump ->
            { destination = (snd jump);
                origin = currLocation;
                jump = true;
                jumpLocations = [(fst jump)] }) //pick the jump location

    let getLegalMoves (board: Board, player: Player): Move list =
        //process the board and find non jump moves for current player
        let playerSquares =
            board
            |> List.concat
            |> List.filter (isSquareOccupiedByPlayer player)

        let jumpMoves =
            playerSquares
            |> List.collect (fun square -> generatePossibleJumpMoves (board, square))

        match jumpMoves |> List.length with
        | x when x > 0 -> jumpMoves
        | x when x <= 0 ->
            playerSquares
            |> List.collect (fun square -> generatePossibleNonJumpMoves (board, square))
        | _ -> []


    let getNextPlayer player =
        match player with
        | Red -> Black
        | Black -> Red

    let utcScore parentRollouts childRollouts winPct =
        let exploration =
            Math.Sqrt
                (Math.Log(float parentRollouts)
                    / (float childRollouts))

        winPct + 1.5 * exploration

    let createNode (gameState: GameState) =
        { possibleMoves = (getLegalMoves (gameState.board, gameState.currPlayer));
            children = Map.empty;
            numRollouts = 0;
            gameState = gameState;
            winCounts = Map.empty |> Map.add (Black) 0 |> Map.add (Red) 0 }

    let createNodeFromWinner (gameState: GameState, winner: Player): MCTSNode =
        let node = createNode (gameState)

        { node with
                numRollouts = 1
                winCounts = Map.add winner 1 node.winCounts }

    let unvisitedMoves node =
        getLegalMoves (node.gameState.board, node.gameState.currPlayer)
        |> Seq.filter (fun mv -> not (Map.containsKey mv node.children))

    let canAddChild node = Seq.length (unvisitedMoves node) > 0

    let isTerminal node =
        getLegalMoves (node.gameState.board, node.gameState.currPlayer)
        |> Seq.isEmpty

    let winningPercent node player =
        Map.tryFind player node.winCounts
        |> Option.map (fun i -> (float i) / (float node.numRollouts))
        |> Option.defaultValue 0.0

    let selectChild player node =
        let totalRollouts =
            node.children
            |> Map.fold (fun state _ node -> state + node.numRollouts) 0

        node.children
        |> Map.toList
        |> Seq.map (fun (mv, c) -> ((mv, c), utcScore totalRollouts c.numRollouts (winningPercent node player)))
        |> Seq.maxBy (fun (_, n) -> n)
        |> fst

    let inline replace list a b =
        list |> List.map (fun x -> if x = a then b else x)

    let updateBoard (board:Board, row:int, col:int, pieceChar:char):Board =
        let boardRow = board.[row]

        let newRow =
            replace boardRow boardRow.[col] (getBoardState row col pieceChar)

        replace board board.[row] newRow

    let placeChecker (board: Board, player:Player, move: Move) =
        let { origin = myOrigin; destination = myDestination; jump = myJump; jumpLocations = myJumpLocation } = move
        let (originRow, originCol) = myOrigin
        let (destRow, destCol) = myDestination
        let (piece, _) = board.[originRow].[originCol]

        match piece with
        | None -> board
        | Some piece ->
            let pieceChar =
                match piece with
                | (Red, Soldier) ->
                    match destRow with
                    | 7 -> 'R'
                    | _ -> 'r'
                | (Black, Soldier) -> 
                    match destRow with
                    | 0 -> 'B'
                    | _ -> 'b'
                | (Red, King) -> 'R'
                | (Black, King) -> 'B'

            let newBoard = updateBoard(board, originRow, originCol, '.')
            let finalBoard = updateBoard(newBoard, destRow, destCol, pieceChar)

            //remove jump if necessary
            match myJump with
            | false -> finalBoard
            | true ->
                let (jumpRow, jumpCol) = 
                    match myJumpLocation |> List.length with
                    | 0 -> (0, 0)
                    | _ -> myJumpLocation.[0]

                let finalJumpBoard = updateBoard(finalBoard, jumpRow, jumpCol, '.')

                finalJumpBoard

    let applyMove (gameState: GameState, move: Move option) =
        match move with
        | Some m ->
            let nextBoard = placeChecker (gameState.board, gameState.currPlayer, m)

            { board = nextBoard;
                nextPlayer = getNextPlayer gameState.nextPlayer;
                previousState = Some gameState;
                lastMove = move;
                currPlayer = gameState.nextPlayer }
        | None ->
            { board = gameState.board;
                nextPlayer = getNextPlayer gameState.nextPlayer;
                previousState = Some gameState;
                lastMove = None;
                currPlayer = gameState.nextPlayer }

    let random = Random()

    let getRandomMove node =
        let possibleMoves = unvisitedMoves node

        let index =
            random.Next(Seq.length possibleMoves - 1)

        possibleMoves |> Seq.item index

    let checkCount gameState =
        let redCheckers =
            gameState.board
            |> List.concat
            |> List.filter (isSquareOccupiedByPlayer Red)
            |> List.length

        let blackCheckers =
            gameState.board
            |> List.concat
            |> List.filter (isSquareOccupiedByPlayer Black)
            |> List.length

        { blackCheckers = blackCheckers;
            redCheckers = redCheckers }

    let isOver gameState startTime =
        let count = checkCount gameState
        let currTime = DateTime.Now
        let diffTime = Math.Abs( float((startTime - currTime).Milliseconds))

        match diffTime with
        | (diffTime) when diffTime > 75. -> true
        | _ ->
            match count.blackCheckers, count.redCheckers with
            | (x, y) when x < 1 || y < 1 -> true //not 0 because then you end up in a state where the checkers are just chasing
            | _ -> false

    let selectRandomMove (gameState: GameState): Move option =
        let rnd = System.Random()

        let candidates =
            getLegalMoves (gameState.board, gameState.currPlayer)

        match List.isEmpty candidates with
        | true -> None
        | false ->
            let randomIndex = rnd.Next(List.length candidates - 1)
            Some(List.item randomIndex candidates)

    let getGameResult (gameState: GameState) =
        let count = checkCount gameState

        let playerWin =
            match count.blackCheckers > count.redCheckers with
            | false -> Black
            | true -> Red

        { winner = playerWin;
            winningMargin = Math.Abs(float (count.redCheckers - count.blackCheckers)) }

    let simulateRandomGame (gameState: GameState, startTime:DateTime) =
        let rec play game =
            match (isOver game startTime) with
            | true ->
                let gameResult = getGameResult game
                gameResult.winner
            | false ->
                let move = selectRandomMove game
                play (applyMove (game, move))

        play gameState


    let rec private updateWinningState node child move winner =
        let prevCount = Map.find winner node.winCounts

        { node with
                children = Map.add move child node.children
                numRollouts = node.numRollouts + 1
                winCounts = Map.add winner (prevCount + 1) node.winCounts }

    let rec select node startTime =
        let currTime = DateTime.Now
        let diffTime = Math.Abs( float((startTime - currTime).Milliseconds))

        match diffTime with
        | (diffTime) when diffTime > 85. ->
            let gameResult = getGameResult node.gameState
            (gameResult.winner, node)
        | _ ->
            match not (canAddChild node), not (isTerminal node) with
            | true, true ->
                let (move, child) =
                    selectChild (getNextPlayer node.gameState.currPlayer) node

                let (winner, expanded) = select child startTime
                (winner, updateWinningState node expanded move winner)
            | false, _ ->
                let move = getRandomMove node

                let nextState = applyMove (node.gameState, Some move)

                let winner = simulateRandomGame (nextState, startTime)
                eprintfn "%A" winner
                let child = createNodeFromWinner (nextState, winner)
                (winner, updateWinningState node child move winner)
            | _, _ ->
                let gameResult = getGameResult node.gameState
                (gameResult.winner, node)

    let selectMove (board: Board, player: Player, numRounds: int) =
        let startTime = DateTime.Now
        let gameState =
            { board = board;
                nextPlayer = getNextPlayer player;
                currPlayer = player;
                lastMove = None;
                previousState = None }

        let hasNoLegalMoves =
            getLegalMoves (board, player) |> Seq.isEmpty

        let nextPlayer = getNextPlayer player

        match hasNoLegalMoves with
        | true -> None
        | false ->
            let root =
                seq { 1 .. numRounds }
                |> Seq.fold (fun node _ -> select node startTime |> snd) (createNode gameState)

            root.children
            |> Map.toSeq
            |> Seq.map (fun (move, child) -> (winningPercent child nextPlayer, move))
            |> Seq.maxBy (fun (p, _) -> p)
            |> (fun (_, move) -> Some move)

    let play (board: Board, player: Player) =
        let move = selectMove (board, player, 100)

        match move with
        | None -> ""
        | Some move -> (convertLocationToString move.origin) + (convertLocationToString move.destination)