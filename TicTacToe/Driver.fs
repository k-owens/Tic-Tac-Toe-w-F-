module Driver
open GamePlayer
open UserSelection
open Game


let enterInput () =
    System.Console.ReadLine()


let keepWindowOpen () =
    printfn "Enter any key to exit."
    System.Console.ReadKey() |> ignore
    0


let printToScreen (output) =
    printfn "%s" output


let rec runGame (print: (string -> unit), input : (unit -> string)) =
    let newGame = startNewGame(input, print)
    let over = playGame(newGame)
    if askIfReplay (print, input) then
        runGame(print, input)
    else
        0


[<EntryPoint>]
let main (args : string[]) =
    let over = runGame(printToScreen, enterInput)
    keepWindowOpen()