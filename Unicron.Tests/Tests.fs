module Tests

open System
open Xunit
open Unicron.Checkers
open Xunit.Abstractions

type MyTests(output: ITestOutputHelper) =

    let initialBoard =
        [ ".r.r.r.r"
          "r.r.r.r."
          ".r.r.r.r"
          "........"
          "........"
          "b.b.b.b."
          ".b.b.b.b"
          "b.b.b.b." ]

    [<Fact>]
    let ``A move is selected`` () =
        let board = parseBoard initialBoard
        let move = play (board, Red)

        let hasMove = not (String.IsNullOrEmpty move)
        Assert.Equal(true, hasMove)
    
    [<Fact>]
    let ``King me`` () =
        let kingMeBoard =
            [ ".r...r.r"
              "r.r.r.r."
              ".r.r.b.r"
              "........"
              "........"
              "b.b.b.b."
              ".b.b.b.b"
              "b.b.b.b." ]
        let board = parseBoard kingMeBoard
        let move = play (board, Black)

        Assert.Equal("F6D8", move)
    
    [<Fact>]
    let ``Test infinite loop`` () =
        let board =
            [ "...r.r.r"
              "r.r.r.r."
              "...r.r.r"
              "..r....."
              ".....b.b"
              "b.r.b.b."
              ".b.....b"
              "b.b.b.b." ]
        let board = parseBoard board
        let move = play (board, Black)

        Assert.Equal("B2D4", move)

    [<Fact>]
    let ``An empty board position is parsed`` () =
        let boardState = parseBoardLine 0 "........"
        let head = boardState |> List.head
        Assert.Equal(8, boardState |> Seq.length)
        Assert.Equal((None, (0, 0)), head)

    [<Fact>]
    let ``A red soldier is parsed`` () =
        let boardState = parseBoardLine 0 "r......."
        let head = boardState |> List.head
        Assert.Equal((Some(Red, Soldier), (0, 0)), head)

    [<Fact>]
    let ``A black soldier is parsed`` () =
        let boardState = parseBoardLine 0 "b......."
        let head = boardState |> List.head
        Assert.Equal((Some(Black, Soldier), (0, 0)), head)

    [<Fact>]
    let ``A red king is parsed`` () =
        let boardState = parseBoardLine 0 "R......."
        let head = boardState |> List.head
        Assert.Equal((Some(Red, King), (0, 0)), head)

    [<Fact>]
    let ``A black king is parsed`` () =
        let boardState = parseBoardLine 0 "B......."
        let head = boardState |> List.head
        Assert.Equal((Some(Black, King), (0, 0)), head)

    [<Fact>]
    let ``Invalid state is parsed correctly`` () =
        let boardState = parseBoardLine 0 "Z......."
        let head = boardState |> List.head
        Assert.Equal((None, (0, 0)), head)

    [<Fact>]
    let ``Full board is parsed correctly`` () =
        let boardStrings = [ for i in 1 .. 8 -> "........" ]
        let board = parseBoard boardStrings
        let head = board |> List.head |> List.head
        Assert.Equal((None, (0, 0)), head)

    [<Fact>]
    let ``Is square occupied by red`` () =
        let boardStrings = [ for i in 1 .. 8 -> "r......." ]
        let board = parseBoard boardStrings
        let head = board |> List.head |> List.head
        let isOccupied = isSquareOccupiedByPlayer Red head
        Assert.Equal(true, isOccupied)

    [<Fact>]
    let ``Is square occupied by red king`` () =
        let boardStrings = [ for i in 1 .. 8 -> "R......." ]
        let board = parseBoard boardStrings
        let head = board |> List.head |> List.head
        let isOccupied = isSquareOccupiedByPlayer Red head
        Assert.Equal(true, isOccupied)

    [<Fact>]
    let ``Is square not occupied by red`` () =
        let boardStrings = [ for i in 1 .. 8 -> "b......." ]
        let board = parseBoard boardStrings
        let head = board |> List.head |> List.head
        let isOccupied = isSquareOccupiedByPlayer Red head
        Assert.Equal(false, isOccupied)

    [<Fact>]
    let ``Get legal moves produces two for red`` () =
        let board = parseBoard initialBoard
        let moves = getLegalMoves (board, Red)
        output.WriteLine("available red moves {0}", sprintf "%A" moves)
        Assert.Equal(7, moves |> List.length)

    [<Fact>]
    let ``Red soldier can not move backwards`` () =
        let redSetup =
            [ ".r.r.r.r"
              "r.r.r.r."
              "...r.r.r"
              "r......."
              "........"
              "b.b.b.b."
              ".b.b.b.b"
              "b.b.b.b." ]

        let board = parseBoard redSetup
        let moves = getLegalMoves (board, Red)
        output.WriteLine("available red moves {0}", sprintf "%A" moves)
        Assert.Equal(8, moves |> List.length)

    [<Fact>]
    let ``Black soldier can not move backwards`` () =
        let redSetup =
            [ ".r.r.r.r"
              "r.r.r.r."
              "...r.r.r"
              "r......."
              ".b......"
              "b...b.b."
              ".b.b.b.b"
              "b.b.b.b." ]

        let board = parseBoard redSetup
        let moves = getLegalMoves (board, Black)
        output.WriteLine("available black moves {0}", sprintf "%A" moves)
        Assert.Equal(7, moves |> List.length)

    [<Fact>]
    let ``Red soldier can jump forward`` () =
        let redSetup =
            [ ".r.r.r.r"
              "r.r.r.r."
              "...r.r.r"
              "r......."
              ".b......"
              "b...b.b."
              ".b.b.b.b"
              "b.b.b.b." ]

        let board = parseBoard redSetup
        let moves = getLegalMoves (board, Red)
        output.WriteLine("available red moves {0}", sprintf "%A" moves)
        Assert.Equal(1, moves |> List.length)

    [<Fact>]
    let ``Red king can jump backward`` () =
        let redSetup =
            [ ".r.r.r.r"
              "r...r.r."
              ".b.r.r.r"
              "R......."
              "........"
              "b...b.b."
              ".b.b.b.b"
              "b.b.b.b." ]

        let board = parseBoard redSetup
        let moves = getLegalMoves (board, Red)
        output.WriteLine("available red moves {0}", sprintf "%A" moves)
        Assert.Equal(2, moves |> List.length)
