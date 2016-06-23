module Driver

open System
open GamePlayer


[<EntryPoint>]
let main (args : string[]) = 
    let startingBoard = [ ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ' ]
    printfn "Tic-Tac-Toe"

    playGame (startingBoard, 1, -1, -1)
    0
