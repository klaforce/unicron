module Tests

open System
open Xunit
open Unicron.Checkers

[<Fact>]
let ``An empty board position is parsed`` () =
    let boardState = parseBoardLine "........"
    let head = boardState |> Seq.head
    Assert.Equal(8, boardState |> Seq.length)
    Assert.Equal(None, head)

[<Fact>]
let ``A red soldier is parsed`` () =
    let boardState = parseBoardLine "r......."
    let head = boardState |> Seq.head
    Assert.Equal(Some (Red, Soldier), head)

[<Fact>]
let ``A black soldier is parsed`` () =
    let boardState = parseBoardLine "b......."
    let head = boardState |> Seq.head
    Assert.Equal(Some (Black, Soldier), head)

[<Fact>]
let ``A red king is parsed`` () =
    let boardState = parseBoardLine "R......."
    let head = boardState |> Seq.head
    Assert.Equal(Some (Red, King), head)

[<Fact>]
let ``A black king is parsed`` () =
    let boardState = parseBoardLine "B......."
    let head = boardState |> Seq.head
    Assert.Equal(Some (Black, King), head)

[<Fact>]
let ``Invalid state is parsed correctly`` () =
    let boardState = parseBoardLine "Z......."
    let head = boardState |> Seq.head
    Assert.Equal(None, head)

[<Fact>]
let ``Full board is parsed correctly`` () =
    let board = Seq.init 5 (fun n -> "........")
    let boardState = parseBoard board
    let head = boardState |> Seq.head |> Seq.head
    Assert.Equal(None, head)


