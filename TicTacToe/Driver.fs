module Driver
(*
open System
open GamePlayer3X3
open GamePlayer4X4

let keepWindowOpen () =
    System.Console.ReadKey() |> ignore

let rec getCharacterInput () : char list =
    printfn "Please enter the character for your moves:"
    let userCharacter = System.Console.ReadKey().KeyChar
    printfn ""

    printfn "Please enter the character for the computer's moves:"
    let compCharacter = System.Console.ReadKey().KeyChar
    printfn ""

    if(not(userCharacter = ' ' || compCharacter = ' ' || userCharacter = '\r' || compCharacter = '\r' || userCharacter = compCharacter)) then
        let returnChars = [userCharacter; compCharacter]
        returnChars
    else
        printfn "Invalid characters.  Please do not use the same character for both, use spaces, or the enter key."
        getCharacterInput()

[<EntryPoint>]
let main (args : string[]) =
    let startingBoard = [' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']
    printfn "Tic-Tac-Toe"

    let characterList = getCharacterInput()


    printfn "You can go first or the computer can go first.  If you would like to go first please enter '1', otherwise enter any other key."
    let whoGoesFirst = System.Console.ReadKey().KeyChar
    printfn ""
    let doesHumanGoFirst : bool = not(whoGoesFirst = '1')

    printfn"If you would like the board input to look like this please type '1':"
    printfn "7|8|9            1|2|3|4"
    printfn "_____            _______"
    printfn "4|5|6     or     Q|W|E|R"
    printfn "_____            _______"
    printfn "1|2|3            A|S|D|F"
    printfn "                 _______"
    printfn "                 Z|X|C|V"
    printfn ""
    printfn"Or if you would like the board input to be inverted please press any other key:"
    printfn "1|2|3            Z|X|C|V"
    printfn "_____            _______"
    printfn "4|5|6     or     A|S|D|F"
    printfn "_____            _______"
    printfn "7|8|9            Q|W|E|R"
    printfn "                 _______"
    printfn "                 1|2|3|4"
    printfn ""
    let boardOrientation = System.Console.ReadKey().KeyChar
    let isBoardTopHeavy = (boardOrientation = '1')

    printfn "Would you like to play on a 3X3 board or 4X4 board? Enter '3' for 3X3."
    let boardSize = System.Console.ReadKey().KeyChar
    let isBoard3 = boardSize = '3'

    if(isBoard3) then
        displayBoardState(playGame3X3 (startingBoard, 1, -1, -1, characterList.[0], characterList.[1], doesHumanGoFirst,isBoardTopHeavy))
    else
        displayBoardState4X4(playGame4X4 (startingBoard,1, characterList.[0], characterList.[1], doesHumanGoFirst,isBoardTopHeavy))


    printfn "The game is over.  The computer is still unbeaten."
    printfn "Enter any key to exit."
    keepWindowOpen ()
    0*)