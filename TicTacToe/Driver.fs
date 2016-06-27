module Driver

open System
open GamePlayer


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


    if(whoGoesFirst = '1') then
        playGame (startingBoard, 1, -1, -1, userCharacter, compCharacter, false)
    else
        playGame (startingBoard, 1, -1, -1, userCharacter, compCharacter, true)
    0
