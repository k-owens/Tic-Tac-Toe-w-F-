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

let askIfGameOver (print, input : unit -> string) = 
    print ("If you would like to play again please press the Y key.")
    let answer = input().ToCharArray()
    answer.[0] = 'y' || answer.[0] = 'Y'


let rec runGame (print, input, size, doesComputerGoFirst, characters : char list) =
    let x = startNewGame(size,doesComputerGoFirst,characters.[0], characters.[1], enterInput, printToScreen)
    if askIfGameOver (print, input) then
        runGame(print,input, size, doesComputerGoFirst, characters)
    else
        0


[<EntryPoint>]
let main (args : string[]) =
    let size = getSizeOfBoard (printToScreen, enterInput)
    let doesComputerGoFirst = doesComputerGoFirst (printToScreen, enterInput)
    let characters = getCharacterInput(printToScreen,enterInput)
    let over = runGame(printToScreen,enterInput, size, doesComputerGoFirst, characters)
    keepWindowOpen()

   //need to add inverted option