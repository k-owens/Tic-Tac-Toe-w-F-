module Driver

open System
open GamePlayer


[<EntryPoint>]
let main (args : string[]) = 
    let startingBoard = [ ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']
    printfn "Tic-Tac-Toe"

    printfn "Please enter the character for your moves:"
    let userCharacter = System.Console.ReadKey().KeyChar

    printfn "Please enter the character for the computer's moves:"
    let compCharacter = System.Console.ReadKey().KeyChar

    playGame (startingBoard, 1, -1, -1, userCharacter, compCharacter)
    0
