module Driver
open GamePlayer
open UserSelection


let enterInput () =
    System.Console.ReadLine()


let keepWindowOpen () =
    printfn "The game is over.  The computer is still unbeaten."
    printfn "Enter any key to exit."
    System.Console.ReadKey() |> ignore

let printToScreen (output) =
    printfn "%s" output

[<EntryPoint>]
let main (args : string[]) =
    let size = getSizeOfBoard (printToScreen, enterInput)
    let doesComputerGoFirst = doesComputerGoFirst (printToScreen, enterInput)
    let characters = getCharacterInput(printToScreen,enterInput)
    let x = startNewGame(size,doesComputerGoFirst,characters.[0], characters.[1], enterInput, printToScreen)
    keepWindowOpen()
    0

   //need to add inverted option