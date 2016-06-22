module GamePlayer

let middleSquare = 5
let outsideSquares = [| 2; 4; 6; 8 |]
let cornerSquares = [| 1; 3; 7; 9 |]

let makeMove (gameState : char[], player : char, place : int) : char[] = 
    let (newGameState : char[]) = gameState
    if(newGameState.[place-1] = ' ') then
        newGameState.[place-1] <- player
        newGameState
    else
        gameState

let displayBoardState (gameState : char[]) =
    printfn ""
    printfn "Current board state:"
    printfn "%c|%c|%c" gameState.[6] gameState.[7] gameState.[8]
    printfn "_____"
    printfn "%c|%c|%c" gameState.[3] gameState.[4] gameState.[5]
    printfn "_____"
    printfn "%c|%c|%c" gameState.[0] gameState.[1] gameState.[2]
    
let askForInput () =
    printfn "Please enter a number to make your next move:"
    printfn ""
    printfn "7|8|9"
    printfn "_____"
    printfn "4|5|6"
    printfn "_____"
    printfn "1|2|3"
    printfn ""


let didTieHappen (gameState : char[]) : bool = 
    if(not(gameState.[0] = ' ') && not(gameState.[1] = ' ') && not(gameState.[2] = ' ') && not(gameState.[3] = ' ') && not(gameState.[4] = ' ') && not(gameState.[5] = ' ') && not(gameState.[6] = ' ') && not(gameState.[7] = ' ') && not(gameState.[8] = ' ')) then
        true
    else
        false

let didOWin (gameState) : bool =
    match gameState with
    | [|'O';'O';'O';_;_;_;_;_;_|] -> true
    | [|_;_;_;'O';'O';'O';_;_;_|] -> true
    | [|_;_;_;_;_;_;'O';'O';'O'|] -> true
    | [|'O';_;_;'O';_;_;'O';_;_|] -> true
    | [|_;'O';_;_;'O';_;_;'O';_|] -> true
    | [|_;_;'O';_;_;'O';_;_;'O'|] -> true
    | [|'O';_;_;_;'O';_;_;_;'O'|] -> true
    | [|_;_;'O';_;'O';_;'O';_;_|] -> true
    | _ -> false

let didXWin (gameState) : bool =
    match gameState with
    | [|'X';'X';'X';_;_;_;_;_;_|] -> true
    | [|_;_;_;'X';'X';'X';_;_;_|] -> true
    | [|_;_;_;_;_;_;'X';'X';'X'|] -> true
    | [|'X';_;_;'X';_;_;'X';_;_|] -> true
    | [|_;'X';_;_;'X';_;_;'X';_|] -> true
    | [|_;_;'X';_;_;'X';_;_;'X'|] -> true
    | [|'X';_;_;_;'X';_;_;_;'X'|] -> true
    | [|_;_;'X';_;'X';_;'X';_;_|] -> true
    | _ -> false

let isGameOver (gameState : char[]) : bool = 
    if (didTieHappen(gameState) || didOWin(gameState) || didXWin(gameState)) then
        true
    else
        false

let rec humanMove (moveNum : int, gameState : char[]) : char[] =

    if moveNum <> 0 && gameState.[moveNum - 1] = ' '  then
        makeMove(gameState, 'O', moveNum)
    else
        printfn "Not a legal move.  Please enter another move:"
        humanMove (System.Convert.ToInt32(System.Console.ReadKey().KeyChar) - System.Convert.ToInt32('0'), gameState)

let winGameOrBlockWin (userCharacter: char, gameState : char[]) : int = 
    let mutable returnNum = -2
    //horizontal
    for i in 0 .. 3 .. 8 do
        if(gameState.[i] = ' ' && gameState.[i+1] = userCharacter && gameState.[i+2] = userCharacter) then
            returnNum <- i
        elif(gameState.[i] = userCharacter && gameState.[i+1] = ' ' && gameState.[i+2] = userCharacter) then
            returnNum <- i + 1
        elif(gameState.[i] = userCharacter && gameState.[i+1] = userCharacter && gameState.[i+2] = ' ') then
            returnNum <- i + 2
    //vertical
    for j in 0 .. 2 do
        if(gameState.[j] = ' ' && gameState.[j+3] = userCharacter && gameState.[j+6] = userCharacter) then
            returnNum <- j
        elif(gameState.[j] = userCharacter && gameState.[j+3] = ' ' && gameState.[j+6] = userCharacter) then
            returnNum <- j + 3
        elif(gameState.[j] = userCharacter && gameState.[j+3] = userCharacter && gameState.[j+6] = ' ') then
            returnNum <- j + 6
    //diagonals
    if(gameState.[0] = ' ' && gameState.[4] = userCharacter && gameState.[8] = userCharacter) then
        returnNum <- 0
    elif(gameState.[0] = userCharacter && gameState.[4] = ' ' && gameState.[8] = userCharacter) then
        returnNum <- 4
    elif(gameState.[0] = userCharacter && gameState.[4] = userCharacter && gameState.[8] = ' ') then
        returnNum <- 8

    if(gameState.[2] = ' ' && gameState.[4] = userCharacter && gameState.[6] = userCharacter) then
        returnNum <- 2
    elif(gameState.[2] = userCharacter && gameState.[4] = ' ' && gameState.[6] = userCharacter) then
        returnNum <- 4
    elif(gameState.[2] = userCharacter && gameState.[4] = userCharacter && gameState.[6] = ' ') then
        returnNum <- 6
    returnNum + 1


let chooseCorner (gameState : char[]) : int = 
    if(gameState.[cornerSquares.[1] - 1] = ' ') then
        cornerSquares.[1]
    elif(gameState.[cornerSquares.[2] - 1] = ' ') then
        cornerSquares.[2]
    elif(gameState.[cornerSquares.[3] - 1] = ' ') then
        cornerSquares.[3]
    elif(gameState.[cornerSquares.[0] - 1] = ' ') then
        cornerSquares.[0]
    else
        -1

let chooseSide (gameState : char[]) : int = 
    if(gameState.[outsideSquares.[0] - 1] = ' ') then
        outsideSquares.[0]
    elif(gameState.[outsideSquares.[1] - 1] = ' ') then
        outsideSquares.[1]
    elif(gameState.[outsideSquares.[2] - 1] = ' ') then
        outsideSquares.[2]
    elif(gameState.[outsideSquares.[3]] = ' ') then
        outsideSquares.[3]
    else
        -1

let chooseCornerInBetween (huMove : int, gameState : char[], firstHumanMove : int) : int=
    if((firstHumanMove = cornerSquares.[0] && huMove = cornerSquares.[3]) || (firstHumanMove = cornerSquares.[3] && huMove = cornerSquares.[0]) || (firstHumanMove = cornerSquares.[1] && huMove = cornerSquares.[2]) || (firstHumanMove = cornerSquares.[2] && huMove = cornerSquares.[14])) then
        chooseSide (gameState)
    elif((firstHumanMove = outsideSquares.[0] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[2])) || (firstHumanMove = cornerSquares.[1] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[2])) || (firstHumanMove = outsideSquares.[1] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[1])) || (firstHumanMove = cornerSquares.[2] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[1]))) then
        cornerSquares.[0]
    elif((firstHumanMove = outsideSquares.[0] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[3])) || (firstHumanMove = cornerSquares.[0] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[3])) || (firstHumanMove = outsideSquares.[2] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[0])) || (firstHumanMove = cornerSquares.[3] && (huMove = outsideSquares.[0] || huMove = cornerSquares.[0]))) then
        cornerSquares.[1]
    elif((firstHumanMove = outsideSquares.[1] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[3])) || (firstHumanMove = cornerSquares.[0] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[3])) || (firstHumanMove = outsideSquares.[3] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[0])) || (firstHumanMove = cornerSquares.[3] && (huMove = outsideSquares.[1] || huMove = cornerSquares.[0]))) then
        cornerSquares.[2]
    elif((firstHumanMove = outsideSquares.[2] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[2])) || (firstHumanMove = cornerSquares.[1] && (huMove = outsideSquares.[3] || huMove = cornerSquares.[2])) || (firstHumanMove = outsideSquares.[3] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[1])) || (firstHumanMove = cornerSquares.[2] && (huMove = outsideSquares.[2] || huMove = cornerSquares.[1]))) then
        cornerSquares.[3]
    else
        -1

let isFirstComputerTurn (gameState : char[]) = 
    match gameState with
    | [|'O';' ';' ';' ';' ';' ';' ';' ';' '|] -> true
    | [|' ';'O';' ';' ';' ';' ';' ';' ';' '|] -> true
    | [|' ';' ';'O';' ';' ';' ';' ';' ';' '|] -> true
    | [|' ';' ';' ';'O';' ';' ';' ';' ';' '|] -> true
    | [|' ';' ';' ';' ';'O';' ';' ';' ';' '|] -> true
    | [|' ';' ';' ';' ';' ';'O';' ';' ';' '|] -> true
    | [|' ';' ';' ';' ';' ';' ';'O';' ';' '|] -> true
    | [|' ';' ';' ';' ';' ';' ';' ';'O';' '|] -> true
    | [|' ';' ';' ';' ';' ';' ';' ';' ';'O'|] -> true
    | _ -> false

let computerMove (gameState : char[], humanMoveSpot : int, firstHumanMove : int) : char[] =
    printfn "Computer move."
    System.Threading.Thread.Sleep(2000)
    let mutable computerMove = -1

    //first move middle
    if(humanMoveSpot = middleSquare && isFirstComputerTurn(gameState)) then
        computerMove <- cornerSquares.[0]
    //first move corner
    elif((humanMoveSpot = cornerSquares.[0] || humanMoveSpot = cornerSquares.[1] || humanMoveSpot = cornerSquares.[2] || humanMoveSpot = cornerSquares.[3]) && isFirstComputerTurn(gameState)) then
        computerMove <- middleSquare
    //first move side
    elif(isFirstComputerTurn(gameState)) then
        computerMove <- middleSquare
    //first move was middle
    elif(firstHumanMove = middleSquare) then
        computerMove <- winGameOrBlockWin ('X', gameState)
        if(computerMove = -1) then
            computerMove <- winGameOrBlockWin ('O', gameState)
        if(computerMove = -1) then
            computerMove <- chooseCorner (gameState)
            if(computerMove = -1) then
                computerMove <- chooseSide (gameState)
    //first move was corner or side
    elif(firstHumanMove <> middleSquare) then
        computerMove <- winGameOrBlockWin ('X', gameState)
        if(computerMove = -1) then
            computerMove <- winGameOrBlockWin ('O', gameState)
        if(computerMove = -1) then
            computerMove <- chooseCornerInBetween (humanMoveSpot, gameState, firstHumanMove)
            if(computerMove = -1) then
                computerMove <- chooseCorner (gameState)
                if(computerMove = -1) then
                    computerMove <- chooseSide (gameState)

    makeMove(gameState,'X',computerMove)

let keepWindowOpen () =
    System.Console.ReadKey() |> ignore

let endGame (gameState : char[]) = 
    displayBoardState (gameState)
    printfn "The game is over.  The computer is still unbeaten."
    keepWindowOpen ()
    exit 0

let rec playGame (gameState : char[], turn : int, humanMoveNum : int, firstMove : int) =
        displayBoardState (gameState)



        if turn % 2 = 1 then
            askForInput ()
            let input = System.Convert.ToInt32(System.Console.ReadKey().KeyChar) - System.Convert.ToInt32('0')
            let newGameState = humanMove (input, gameState)

            if turn = 1 then
                playGame (newGameState, turn + 1, input, input)

            if isGameOver (newGameState) then
                endGame (newGameState)
            playGame (newGameState, turn + 1, input, firstMove)
        else
            
            let newGameState = computerMove (gameState, humanMoveNum, firstMove)
            if isGameOver (newGameState) then
                endGame (newGameState)
            playGame (newGameState, turn + 1, humanMoveNum, firstMove)