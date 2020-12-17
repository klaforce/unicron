let myColor = Console.In.ReadLine() (* r or b *)

let myPlayer = match myColor with
                | "r" -> Checkers.Red
                | "b" -> Checkers.Black
                | _ -> Checkers.Red
(* game loop *)
while true do
    let initialBoard = List.init 8 (fun n -> Console.In.ReadLine())

    let legalMoves = int(Console.In.ReadLine()) (* number of legal moves *)
    let movesAvailable = List.init legalMoves (fun n -> Console.In.ReadLine())
    let multiJumps = movesAvailable |> List.filter (fun n -> n |> String.length > 4) |> List.length

    (* Write an action using printfn *)
    (* To debug: eprintfn "Debug message" *)
    
    let board = Checkers.parseBoard initialBoard
    let move = match multiJumps with
                | x when x = 0 -> 
                    //randomly select a move 10% of the time in order to get out of loops
                    let rnd = System.Random()
                    let pct = rnd.Next(100)
                    match pct with
                    | x when x < 2 -> 
                        let randomIndex = rnd.Next(legalMoves - 1)
                        List.item randomIndex movesAvailable
                    | _ -> Checkers.play (board, myPlayer)
                | x when x > 0 -> movesAvailable |> List.maxBy (fun n -> n |> String.length)
                | _ -> ""

    printfn "%s" move
    ()