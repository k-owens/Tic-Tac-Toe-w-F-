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

let rec runGame (print, input, size,isInverted,player1 : Player,player2) =
    let x = startNewGame(size,player1.PlayerCharacter, player2.PlayerCharacter, enterInput, printToScreen,isInverted, player1, player2)
    if askIfGameOver (print, input) then
        runGame(print,input, size, isInverted, player1, player2)
    else
        0


[<EntryPoint>]
let main (args : string[]) =
    let size = getSizeOfBoard (printToScreen, enterInput)
    let isInverted = askIfInverted(printToScreen,enterInput)
    let player1Info = askForPlayerInformation(printToScreen,enterInput,1,size)
    let player2Info = askForPlayerInformation(printToScreen,enterInput,2,size)
    let playerCharacters = getCharacterInput(printToScreen,enterInput)
    let player1 = {PlayerSeed = player1Info.PlayerSeed; ComputerAlgorithm = player1Info.ComputerAlgorithm; PlayerCharacter = playerCharacters.[0]}
    let player2 = {PlayerSeed = player2Info.PlayerSeed; ComputerAlgorithm = player2Info.ComputerAlgorithm; PlayerCharacter = playerCharacters.[1]}
    let over = runGame(printToScreen,enterInput, size,isInverted,player1,player2)
    keepWindowOpen()