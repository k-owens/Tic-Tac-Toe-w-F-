// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.]
open System


[<EntryPoint>]
let main (args : string[]) = 
    let (squareChosen: char[]) = [| ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ' |]
    let middleSquare = 5
    let outsideSquares = [| 2; 4; 6; 8 |]
    let cornerSquares = [| 1; 3; 7; 9 |]

    printfn "Tic-Tac-Toe"

    let mutable isGameOver = false

    while not(isGameOver) do
        (* Display the board to the person playing*)
        printfn ""
        printfn "Current board state:"
        printfn "%c|%c|%c" squareChosen.[0] squareChosen.[1] squareChosen.[2]
        printfn "_____"
        printfn "%c|%c|%c" squareChosen.[3] squareChosen.[4] squareChosen.[5]
        printfn "_____"
        printfn "%c|%c|%c" squareChosen.[6] squareChosen.[7] squareChosen.[8]
        
        printfn ""
        printfn "Please enter a number to make your next move:"
        printfn ""
        printfn "1|2|3"
        printfn "_____"
        printfn "4|5|6"
        printfn "_____"
        printfn "7|8|9"
        (* End of Display code*)

        (* Start of human move*)
        let mutable notGoodMove = true
        while notGoodMove do
            let (humanMove: int) = System.Convert.ToInt32(System.Console.ReadKey().KeyChar) - System.Convert.ToInt32('0')

            if squareChosen.[humanMove - 1] = ' ' then
                squareChosen.[humanMove - 1] <- 'O'
                printfn ""
                notGoodMove <- false
            else
                notGoodMove <- true
                printfn "Not a legal move.  Please enter another move:"

        //check if game is over
        if(squareChosen.[0] = 'O' && squareChosen.[1] = 'O' && squareChosen.[2] = 'O') then
            isGameOver <- true
        elif(squareChosen.[0] = 'X' && squareChosen.[1] = 'X' && squareChosen.[2] = 'X') then
            isGameOver <- true
        elif(squareChosen.[3] = 'O' && squareChosen.[4] = 'O' && squareChosen.[5] = 'O') then
            isGameOver <- true
        elif(squareChosen.[3] = 'X' && squareChosen.[4] = 'X' && squareChosen.[5] = 'X') then
            isGameOver <- true
        elif(squareChosen.[6] = 'O' && squareChosen.[7] = 'O' && squareChosen.[8] = 'O') then
            isGameOver <- true
        elif(squareChosen.[6] = 'X' && squareChosen.[7] = 'X' && squareChosen.[8] = 'X') then
            isGameOver <- true
        elif(squareChosen.[0] = 'O' && squareChosen.[3] = 'O' && squareChosen.[6] = 'O') then
            isGameOver <- true
        elif(squareChosen.[0] = 'X' && squareChosen.[3] = 'X' && squareChosen.[6] = 'X') then
            isGameOver <- true
        elif(squareChosen.[1] = 'O' && squareChosen.[4] = 'O' && squareChosen.[7] = 'O') then
            isGameOver <- true
        elif(squareChosen.[1] = 'X' && squareChosen.[4] = 'X' && squareChosen.[7] = 'X') then
            isGameOver <- true
        elif(squareChosen.[2] = 'O' && squareChosen.[5] = 'O' && squareChosen.[8] = 'O') then
            isGameOver <- true
        elif(squareChosen.[2] = 'X' && squareChosen.[5] = 'X' && squareChosen.[8] = 'X') then
            isGameOver <- true
        elif(squareChosen.[0] = 'O' && squareChosen.[4] = 'O' && squareChosen.[8] = 'O') then
            isGameOver <- true
        elif(squareChosen.[0] = 'X' && squareChosen.[4] = 'X' && squareChosen.[8] = 'X') then
            isGameOver <- true
        elif(squareChosen.[2] = 'O' && squareChosen.[4] = 'O' && squareChosen.[6] = 'O') then
            isGameOver <- true
        elif(squareChosen.[2] = 'X' && squareChosen.[4] = 'X' && squareChosen.[6] = 'X') then
            isGameOver <- true
        elif(not(squareChosen.[0] = ' ') && not(squareChosen.[1] = ' ') && not(squareChosen.[2] = ' ') && not(squareChosen.[3] = ' ') && not(squareChosen.[4] = ' ') && not(squareChosen.[5] = ' ') && not(squareChosen.[6] = ' ') && not(squareChosen.[7] = ' ') && not(squareChosen.[8] = ' ')) then
            isGameOver <- true
        else
            isGameOver <- false



        (* End of human move *)

                (* Display the board to the person playing*)
        printfn ""
        printfn "Current board state:"
        printfn "%c|%c|%c" squareChosen.[0] squareChosen.[1] squareChosen.[2]
        printfn "_____"
        printfn "%c|%c|%c" squareChosen.[3] squareChosen.[4] squareChosen.[5]
        printfn "_____"
        printfn "%c|%c|%c" squareChosen.[6] squareChosen.[7] squareChosen.[8]
        
        System.Threading.Thread.Sleep(1000)

        if not(isGameOver) then
            (* Start of computer move *)
            let mutable notGoodCMove = true
            let mutable computerMove = 9
            while notGoodCMove do
                if squareChosen.[computerMove-1] = ' ' then
                    squareChosen.[computerMove-1] <- 'X'
                    notGoodCMove <- false
                else
                    computerMove <- computerMove - 1
                    notGoodCMove <- true

        //check if game is over
        if(squareChosen.[0] = 'O' && squareChosen.[1] = 'O' && squareChosen.[2] = 'O') then
            isGameOver <- true
        elif(squareChosen.[0] = 'X' && squareChosen.[1] = 'X' && squareChosen.[2] = 'X') then
            isGameOver <- true
        elif(squareChosen.[3] = 'O' && squareChosen.[4] = 'O' && squareChosen.[5] = 'O') then
            isGameOver <- true
        elif(squareChosen.[3] = 'X' && squareChosen.[4] = 'X' && squareChosen.[5] = 'X') then
            isGameOver <- true
        elif(squareChosen.[6] = 'O' && squareChosen.[7] = 'O' && squareChosen.[8] = 'O') then
            isGameOver <- true
        elif(squareChosen.[6] = 'X' && squareChosen.[7] = 'X' && squareChosen.[8] = 'X') then
            isGameOver <- true
        elif(squareChosen.[0] = 'O' && squareChosen.[3] = 'O' && squareChosen.[6] = 'O') then
            isGameOver <- true
        elif(squareChosen.[0] = 'X' && squareChosen.[3] = 'X' && squareChosen.[6] = 'X') then
            isGameOver <- true
        elif(squareChosen.[1] = 'O' && squareChosen.[4] = 'O' && squareChosen.[7] = 'O') then
            isGameOver <- true
        elif(squareChosen.[1] = 'X' && squareChosen.[4] = 'X' && squareChosen.[7] = 'X') then
            isGameOver <- true
        elif(squareChosen.[2] = 'O' && squareChosen.[5] = 'O' && squareChosen.[8] = 'O') then
            isGameOver <- true
        elif(squareChosen.[2] = 'X' && squareChosen.[5] = 'X' && squareChosen.[8] = 'X') then
            isGameOver <- true
        elif(squareChosen.[0] = 'O' && squareChosen.[4] = 'O' && squareChosen.[8] = 'O') then
            isGameOver <- true
        elif(squareChosen.[0] = 'X' && squareChosen.[4] = 'X' && squareChosen.[8] = 'X') then
            isGameOver <- true
        elif(squareChosen.[2] = 'O' && squareChosen.[4] = 'O' && squareChosen.[6] = 'O') then
            isGameOver <- true
        elif(squareChosen.[2] = 'X' && squareChosen.[4] = 'X' && squareChosen.[6] = 'X') then
            isGameOver <- true
        elif(not(squareChosen.[0] = ' ') && not(squareChosen.[1] = ' ') && not(squareChosen.[2] = ' ') && not(squareChosen.[3] = ' ') && not(squareChosen.[4] = ' ') && not(squareChosen.[5] = ' ') && not(squareChosen.[6] = ' ') && not(squareChosen.[7] = ' ') && not(squareChosen.[8] = ' ')) then
            isGameOver <- true
        else
            isGameOver <- false

            (* End of computer move *)

    (* Show ending board*)
    printfn ""
    printfn "Current board state:"
    //print the current board using the bool array
    printfn "%c|%c|%c" squareChosen.[0] squareChosen.[1] squareChosen.[2]
    printfn "_____"
    printfn "%c|%c|%c" squareChosen.[3] squareChosen.[4] squareChosen.[5]
    printfn "_____"
    printfn "%c|%c|%c" squareChosen.[6] squareChosen.[7] squareChosen.[8]
    printfn "The game is over."
    System.Console.ReadKey() |> ignore

    0
