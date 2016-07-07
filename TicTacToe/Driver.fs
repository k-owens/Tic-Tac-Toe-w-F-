module Driver
open GamePlayer
open UserSelection


let enterInput () =
    System.Console.ReadLine()


let keepWindowOpen () =
    printfn "Enter any key to exit."
    System.Console.ReadKey() |> ignore
    0

let printToScreen (output) =
    printfn "%s" output

let rec runGame (print, input, size, doesComputerGoFirst, characters : char list,isInverted,isMinimax) =
    let x = startNewGame(size,doesComputerGoFirst,characters.[0], characters.[1], enterInput, printToScreen,isInverted,isMinimax)
    if askIfGameOver (print, input) then
        runGame(print,input, size, doesComputerGoFirst, characters, isInverted,isMinimax)
    else
        0


[<EntryPoint>]
let main (args : string[]) =
    let size = getSizeOfBoard (printToScreen, enterInput)
    let doesComputerGoFirst = doesComputerGoFirst (printToScreen, enterInput)
    let characters = getCharacterInput(printToScreen,enterInput)
    let isInverted = askIfInverted(printToScreen,enterInput)
    let isMinimax = askIfMinimax(printToScreen,enterInput)
    let over = runGame(printToScreen,enterInput, size, doesComputerGoFirst, characters,isInverted,isMinimax)
    keepWindowOpen()
