open Tests
open Unicron.Checkers

module Program =
    [<EntryPoint>]

    let main _ =
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
        let move = selectMove (board, Black, 1000)
        System.Console.WriteLine(move)
        0
