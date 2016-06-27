module Driver

open System
open GamePlayer

let keepWindowOpen () =
    System.Console.ReadKey() |> ignore

[<EntryPoint>]
let main (args : string[]) = 
    let startingBoard = [ ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']
    printfn "Tic-Tac-Toe"

    printfn "Please enter the character for your moves:"
    let userCharacter = System.Console.ReadKey().KeyChar
    printfn ""

    printfn "Please enter the character for the computer's moves:"
    let compCharacter = System.Console.ReadKey().KeyChar
    printfn ""


    printfn "You can go first or the computer can go first.  If you would like to go first please enter '1', otherwise enter any other key."
    let whoGoesFirst = System.Console.ReadKey().KeyChar
    printfn ""
    let doesHumanGoFirst : bool = not(whoGoesFirst = '1')

    printfn"If you would like the board input to look like this please type '1':"
    printfn "7|8|9"
    printfn "_____"
    printfn "4|5|6"
    printfn "_____"
    printfn "1|2|3"
    printfn ""
    printfn"Or if you would like the board input to look like this please press any other key:"
    printfn "1|2|3"
    printfn "_____"
    printfn "4|5|6"
    printfn "_____"
    printfn "7|8|9"
    let boardOrientation = System.Console.ReadKey().KeyChar
    let isBoardTopHeavy = (boardOrientation = '1')

    displayBoardState(playGame (startingBoard, 1, -1, -1, userCharacter, compCharacter, doesHumanGoFirst,isBoardTopHeavy))

    printfn "The game is over.  The computer is still unbeaten."
    printfn "Enter any key to exit."
    keepWindowOpen ()
    0