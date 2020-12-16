let myColor = Console.In.ReadLine() (* r or b *)

let myPlayer = match myColor with
                | "r" -> Checkers.Red
                | "b" -> Checkers.Black
                | _ -> Checkers.Red
(* game loop *)
while true do
    let initialBoard = List.init 8 (fun n -> Console.In.ReadLine())

    let legalMoves = int(Console.In.ReadLine()) (* number of legal moves *)
    for i in 0 .. legalMoves - 1 do
        let moveString = Console.In.ReadLine() (* move *)
        ()

    
    (* Write an action using printfn *)
    (* To debug: eprintfn "Debug message" *)
    
    let board = Checkers.parseBoard initialBoard
    let move = Checkers.play (board, myPlayer)

    printfn "%s" move
    ()