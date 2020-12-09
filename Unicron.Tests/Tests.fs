module Tests

open System
open Xunit
open Unicron.Checkers

[<Fact>]
let ``A string is parsed`` () =
    let boardState = parseLine "........"
    Assert.Equal(8, boardState.Length)
