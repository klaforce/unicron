open Tests
open Unicron.Checkers

module Program =
    [<EntryPoint>]

    let main _ =
        (*
         let myColor = Console.In.ReadLine() (* r or b *)
let myPlayer = match myColor with
                | "r" -> Red
                | _ -> Black

(* game loop *)
while true do
    let initialBoard = List.init 8 (fun n -> Console.In.ReadLine())

    let legalMoves = int(Console.In.ReadLine()) (* number of legal moves *)
    for i in 0 .. legalMoves - 1 do
        let moveString = Console.In.ReadLine() (* move *)
        ()

    
    (* Write an action using printfn *)
    (* To debug: eprintfn "Debug message" *)
    
    let board = parseBoard initialBoard
    let move = play (board, myPlayer)

    printfn "%s" move
    ()

         let initialBoard =
            [ ".r.r.r.r"
              "r.r.r.r."
              ".r.r.r.r"
              "........"
              "........"
              "b.b.b.b."
              ".b.b.b.b"
              "b.b.b.b." ]

        let board = parseBoard initialBoard
        let move = selectMove (board, Red, 3)
        System.Console.WriteLine(move)*)
        0
