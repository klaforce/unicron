module Tests

open System
open Xunit
open Unicron.Checkers
open Xunit.Abstractions

type MyTests(output:ITestOutputHelper) =

    let initialBoard = [
        ".r.r.r.r";
        "r.r.r.r.";
        ".r.r.r.r";
        "........";
        "........";
        "b.b.b.b.";
        ".b.b.b.b";
        "b.b.b.b."]

    [<Fact>]
    let ``An empty board position is parsed`` () =
        let boardState = parseBoardLine 0 "........"
        let head = boardState |> List.head
        Assert.Equal(8, boardState |> Seq.length)
        Assert.Equal((None,(0,0)), head)

    [<Fact>]
    let ``A red soldier is parsed`` () =
        let boardState = parseBoardLine 0 "r......."
        let head = boardState |> List.head
        Assert.Equal((Some (Red, Soldier),(0,0)), head)

    [<Fact>]
    let ``A black soldier is parsed`` () =
        let boardState = parseBoardLine 0 "b......."
        let head = boardState |> List.head
        Assert.Equal((Some (Black, Soldier),(0,0)), head)

    [<Fact>]
    let ``A red king is parsed`` () =
        let boardState = parseBoardLine 0 "R......."
        let head = boardState |> List.head
        Assert.Equal((Some (Red, King),(0,0)), head)

    [<Fact>]
    let ``A black king is parsed`` () =
        let boardState = parseBoardLine 0 "B......."
        let head = boardState |> List.head
        Assert.Equal((Some (Black, King),(0,0)), head)

    [<Fact>]
    let ``Invalid state is parsed correctly`` () =
        let boardState = parseBoardLine 0 "Z......."
        let head = boardState |> List.head
        Assert.Equal((None,(0,0)), head)

    [<Fact>]
    let ``Full board is parsed correctly`` () =
        let boardStrings = [ for i in 1 .. 8 -> "........" ]
        let board = parseBoard boardStrings
        let head = board |> List.head |> List.head
        Assert.Equal((None,(0,0)), head)

    [<Fact>]
    let ``Is square occupied by red`` () =
        let boardStrings = [ for i in 1 .. 8 -> "r......." ]
        let board = parseBoard boardStrings
        let head = board |> List.head |> List.head
        let isOccupied = isSquareOccupiedByPlayer head Red
        Assert.Equal(true, isOccupied)

    [<Fact>]
    let ``Is square occupied by red king`` () =
        let boardStrings = [ for i in 1 .. 8 -> "R......." ]
        let board = parseBoard boardStrings
        let head = board |> List.head |> List.head
        let isOccupied = isSquareOccupiedByPlayer head Red
        Assert.Equal(true, isOccupied)

    [<Fact>]
    let ``Is square not occupied by red`` () =
        let boardStrings = [ for i in 1 .. 8 -> "b......." ]
        let board = parseBoard boardStrings
        let head = board |> List.head |> List.head
        let isOccupied = isSquareOccupiedByPlayer head Red
        Assert.Equal(false, isOccupied)

    [<Fact>]
    let ``Get red moves should be 0`` () =
        let board = parseBoard initialBoard
        let square = board.[0].[1]
        let moves = getRedMoves board square
        output.WriteLine("starting location {0}", square)
        output.WriteLine("available red moves {0}", moves)
        Assert.Equal(0, moves |> List.length)
    
    [<Fact>]
    let ``Get red moves should be 2`` () =
        let board = parseBoard initialBoard
        let square = board.[2].[1]
        let moves = getRedMoves board square
        output.WriteLine("starting location {0}", square)
        output.WriteLine("available red moves {0}", moves)
        Assert.Equal(2, moves |> List.length)

    [<Fact>]
    let ``Get legal moves produces two for red`` () =
        let board = parseBoard initialBoard
        let head = board |> List.head |> List.head
        let moves = getLegalNonJumps board Red
        Assert.Equal(12, moves |> List.length)


