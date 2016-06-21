open System

let (squareChosen: char[]) = [| ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ' |]
let middleSquare = 5
let outsideSquares = [| 2; 4; 6; 8 |]
let cornerSquares = [| 1; 3; 7; 9 |]
let mutable humanMoveSpot = 0
let mutable turn = 1
let mutable firstHumanMove = -1

let displayBoardState () =
    printfn ""
    printfn "Current board state:"
    printfn "%c|%c|%c" squareChosen.[6] squareChosen.[7] squareChosen.[8]
    printfn "_____"
    printfn "%c|%c|%c" squareChosen.[3] squareChosen.[4] squareChosen.[5]
    printfn "_____"
    printfn "%c|%c|%c" squareChosen.[0] squareChosen.[1] squareChosen.[2]
    
let askForInput () =
    printfn "Please enter a number to make your next move:"
    printfn ""
    printfn "7|8|9"
    printfn "_____"
    printfn "4|5|6"
    printfn "_____"
    printfn "1|2|3"
    printfn ""


let checkHumanWin () : bool =
    if(squareChosen.[0] = 'O' && squareChosen.[1] = 'O' && squareChosen.[2] = 'O') then
        true
    elif(squareChosen.[3] = 'O' && squareChosen.[4] = 'O' && squareChosen.[5] = 'O') then
        true
    elif(squareChosen.[6] = 'O' && squareChosen.[7] = 'O' && squareChosen.[8] = 'O') then
        true
    elif(squareChosen.[0] = 'O' && squareChosen.[3] = 'O' && squareChosen.[6] = 'O') then
        true
    elif(squareChosen.[1] = 'O' && squareChosen.[4] = 'O' && squareChosen.[7] = 'O') then
        true
    elif(squareChosen.[2] = 'O' && squareChosen.[5] = 'O' && squareChosen.[8] = 'O') then
        true
    elif(squareChosen.[0] = 'O' && squareChosen.[4] = 'O' && squareChosen.[8] = 'O') then
        true
    elif(squareChosen.[2] = 'O' && squareChosen.[4] = 'O' && squareChosen.[6] = 'O') then
        true
    elif(not(squareChosen.[0] = ' ') && not(squareChosen.[1] = ' ') && not(squareChosen.[2] = ' ') && not(squareChosen.[3] = ' ') && not(squareChosen.[4] = ' ') && not(squareChosen.[5] = ' ') && not(squareChosen.[6] = ' ') && not(squareChosen.[7] = ' ') && not(squareChosen.[8] = ' ')) then
        true
    else
        false

let checkComputerWin () : bool =
    if(squareChosen.[0] = 'X' && squareChosen.[1] = 'X' && squareChosen.[2] = 'X') then
        true
    elif(squareChosen.[3] = 'X' && squareChosen.[4] = 'X' && squareChosen.[5] = 'X') then
        true
    elif(squareChosen.[6] = 'X' && squareChosen.[7] = 'X' && squareChosen.[8] = 'X') then
        true
    elif(squareChosen.[0] = 'X' && squareChosen.[3] = 'X' && squareChosen.[6] = 'X') then
        true
    elif(squareChosen.[1] = 'X' && squareChosen.[4] = 'X' && squareChosen.[7] = 'X') then
        true
    elif(squareChosen.[2] = 'X' && squareChosen.[5] = 'X' && squareChosen.[8] = 'X') then
        true
    elif(squareChosen.[0] = 'X' && squareChosen.[4] = 'X' && squareChosen.[8] = 'X') then
        true
    elif(squareChosen.[2] = 'X' && squareChosen.[4] = 'X' && squareChosen.[6] = 'X') then
        true
    elif(not(squareChosen.[0] = ' ') && not(squareChosen.[1] = ' ') && not(squareChosen.[2] = ' ') && not(squareChosen.[3] = ' ') && not(squareChosen.[4] = ' ') && not(squareChosen.[5] = ' ') && not(squareChosen.[6] = ' ') && not(squareChosen.[7] = ' ') && not(squareChosen.[8] = ' ')) then
        true
    else
        false

let rec humanMove (moveNum) =
    humanMoveSpot <- moveNum

    if humanMoveSpot <> 0 && squareChosen.[humanMoveSpot - 1] = ' '  then
        squareChosen.[humanMoveSpot - 1] <- 'O'
        printfn ""
    else
        printfn "Not a legal move.  Please enter another move:"
        humanMove (System.Convert.ToInt32(System.Console.ReadKey().KeyChar) - System.Convert.ToInt32('0'))



let winGameOrBlockWin (userCharacter) : int = 
    let mutable returnNum = -2
    //horizontal
    for i in 0 .. 3 .. 8 do
        if(squareChosen.[i] = ' ' && squareChosen.[i+1] = userCharacter && squareChosen.[i+2] = userCharacter) then
            returnNum <- i
        elif(squareChosen.[i] = userCharacter && squareChosen.[i+1] = ' ' && squareChosen.[i+2] = userCharacter) then
            returnNum <- i + 1
        elif(squareChosen.[i] = userCharacter && squareChosen.[i+1] = userCharacter && squareChosen.[i+2] = ' ') then
            returnNum <- i + 2
    //vertical
    for j in 0 .. 2 do
        if(squareChosen.[j] = ' ' && squareChosen.[j+3] = userCharacter && squareChosen.[j+6] = userCharacter) then
            returnNum <- j
        elif(squareChosen.[j] = userCharacter && squareChosen.[j+3] = ' ' && squareChosen.[j+6] = userCharacter) then
            returnNum <- j + 3
        elif(squareChosen.[j] = userCharacter && squareChosen.[j+3] = userCharacter && squareChosen.[j+6] = ' ') then
            returnNum <- j + 6
    //diagonals *****come back and undo brute force using the identifier arrays******
    if(squareChosen.[0] = ' ' && squareChosen.[4] = userCharacter && squareChosen.[8] = userCharacter) then
        returnNum <- 0
    elif(squareChosen.[0] = userCharacter && squareChosen.[4] = ' ' && squareChosen.[8] = userCharacter) then
        returnNum <- 4
    elif(squareChosen.[0] = userCharacter && squareChosen.[4] = userCharacter && squareChosen.[8] = ' ') then
        returnNum <- 8

    if(squareChosen.[2] = ' ' && squareChosen.[4] = userCharacter && squareChosen.[6] = userCharacter) then
        returnNum <- 2
    elif(squareChosen.[2] = userCharacter && squareChosen.[4] = ' ' && squareChosen.[6] = userCharacter) then
        returnNum <- 4
    elif(squareChosen.[2] = userCharacter && squareChosen.[4] = userCharacter && squareChosen.[6] = ' ') then
        returnNum <- 6
    returnNum + 1


let chooseCorner () : int = 
    let mutable returnNum = -1
    if(squareChosen.[cornerSquares.[1] - 1] = ' ') then
        returnNum <-cornerSquares.[1]//choose the corners (index 0 will always be taken)
    elif(squareChosen.[cornerSquares.[2] - 1] = ' ') then
        returnNum <- cornerSquares.[2]
    elif(squareChosen.[cornerSquares.[3] - 1] = ' ') then
        returnNum <- cornerSquares.[3]
    elif(squareChosen.[cornerSquares.[0] - 1] = ' ') then
        returnNum <- cornerSquares.[0]
    returnNum

let chooseSide () : int = 
    let mutable returnNum = -1
    if(squareChosen.[outsideSquares.[0] - 1] = ' ') then
        returnNum <- outsideSquares.[0]
    elif(squareChosen.[outsideSquares.[1] - 1] = ' ') then
        returnNum <- outsideSquares.[1]
    elif(squareChosen.[outsideSquares.[2] - 1] = ' ') then
        returnNum <- outsideSquares.[2]
    elif(squareChosen.[outsideSquares.[3]] = ' ') then
        returnNum <- outsideSquares.[3]
    returnNum

let chooseCornerInBetween (huMove) : int=
    let mutable returnNum = -1
    if((firstHumanMove = cornerSquares.[0] && huMove = cornerSquares.[3]) || (firstHumanMove = cornerSquares.[3] && huMove = cornerSquares.[0]) || (firstHumanMove = cornerSquares.[1] && huMove = cornerSquares.[2]) || (firstHumanMove = cornerSquares.[2] && huMove = cornerSquares.[14])) then
        returnNum <- chooseSide ()
    elif((firstHumanMove = outsideSquares.[0] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[2])) || (firstHumanMove = cornerSquares.[1] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[2])) || (firstHumanMove = outsideSquares.[1] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[1])) || (firstHumanMove = cornerSquares.[2] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[1]))) then
        returnNum <- cornerSquares.[0]
    elif((firstHumanMove = outsideSquares.[0] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[3])) || (firstHumanMove = cornerSquares.[0] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[3])) || (firstHumanMove = outsideSquares.[2] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[0])) || (firstHumanMove = cornerSquares.[3] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[0]))) then
        returnNum <- cornerSquares.[1]
    elif((firstHumanMove = outsideSquares.[1] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[3])) || (firstHumanMove = cornerSquares.[0] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[3])) || (firstHumanMove = outsideSquares.[3] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[0])) || (firstHumanMove = cornerSquares.[3] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[0]))) then
        returnNum <- cornerSquares.[2]
    elif((firstHumanMove = outsideSquares.[2] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[2])) || (firstHumanMove = cornerSquares.[1] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[2])) || (firstHumanMove = outsideSquares.[3] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[1])) || (firstHumanMove = cornerSquares.[2] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[1]))) then
        returnNum <- cornerSquares.[3]

    returnNum

let computerTurn () =
    printfn "Computer move."
    System.Threading.Thread.Sleep(2000)
    //let mutable notGoodCMove = true
    let mutable computerMove = -1

    //first move middle
    if(humanMoveSpot = middleSquare && firstHumanMove = -1) then
        computerMove <- cornerSquares.[0]
        firstHumanMove <- humanMoveSpot
    //first move corner
    elif((humanMoveSpot = cornerSquares.[0] || humanMoveSpot = cornerSquares.[1] || humanMoveSpot = cornerSquares.[2] || humanMoveSpot = cornerSquares.[3]) && firstHumanMove = -1) then
        computerMove <- middleSquare
        firstHumanMove <- humanMoveSpot
    //first move side
    elif(firstHumanMove = -1) then
        computerMove <- middleSquare
        firstHumanMove <- humanMoveSpot
    //first move was middle
    elif(firstHumanMove = middleSquare) then
        computerMove <- winGameOrBlockWin ('X')
        if(computerMove = -1) then
            computerMove <- winGameOrBlockWin ('O')
        if(computerMove = -1) then
            computerMove <- chooseCorner ()
            if(computerMove = -1) then
                computerMove <- chooseSide ()
                //choose remaining sides
    //first move was corner or side
    elif(firstHumanMove <> middleSquare) then
        computerMove <- winGameOrBlockWin ('X')
        if(computerMove = -1) then
            computerMove <- winGameOrBlockWin ('O')
        if(computerMove = -1) then
            computerMove <- chooseCornerInBetween (humanMoveSpot)
            if(computerMove = -1) then
                computerMove <- chooseCorner ()
                if(computerMove = -1) then
                    computerMove <- chooseSide ()
    
    squareChosen.[computerMove-1] <- 'X'

let keepWindowOpen () =
    System.Console.ReadKey() |> ignore



[<EntryPoint>]
let main (args : string[]) = 
    let mutable isGameOver = false

    printfn "Tic-Tac-Toe"

    while not(isGameOver) do
        displayBoardState ()
        askForInput ()

        humanMove (System.Convert.ToInt32(System.Console.ReadKey().KeyChar) - System.Convert.ToInt32('0'))

        isGameOver <- checkHumanWin ()

        System.Threading.Thread.Sleep(1000)

        displayBoardState ()

        if not(isGameOver) then
            computerTurn()
            isGameOver <- checkComputerWin ()
    turn <- turn + 1

    displayBoardState ()
    printfn "The game is over.  The computer is still unbeaten."
    keepWindowOpen ()
    0
