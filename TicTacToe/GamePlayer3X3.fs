module GamePlayer3X3

let middleSquare = 5
let outsideSquares = [ 2; 4; 6; 8 ]
let cornerSquares = [ 1; 3; 7; 9 ]

let makeMove (gameState : char list, player : char, place : int) : char list = 
    if(gameState.[place-1] = ' ') then
        if (place = 1) then
            let newGameState = [player;gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8]]
            newGameState
        elif (place = 2) then
            let newGameState = [gameState.[0];player;gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8]]
            newGameState
        elif (place = 3) then
            let newGameState = [gameState.[0];gameState.[1];player;gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8]]
            newGameState
        elif (place = 4) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];player;gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8]]
            newGameState
        elif (place = 5) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];player;gameState.[5];gameState.[6];gameState.[7];gameState.[8]]
            newGameState
        elif (place = 6) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];player;gameState.[6];gameState.[7];gameState.[8]]
            newGameState
        elif (place = 7) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];player;gameState.[7];gameState.[8]]
            newGameState
        elif (place = 8) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];player;gameState.[8]]
            newGameState
        else
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];player]
            newGameState
    else
    gameState

let displayBoardState (gameState : char list) =
        printfn ""
        printfn "Current board:"
        printfn "%c|%c|%c" gameState.[6] gameState.[7] gameState.[8]
        printfn "_____"
        printfn "%c|%c|%c" gameState.[3] gameState.[4] gameState.[5]
        printfn "_____"
        printfn "%c|%c|%c" gameState.[0] gameState.[1] gameState.[2]
    
let askForInput (isBoardTopHeavy : bool) : int =
    if(isBoardTopHeavy) then
        printfn "Please enter a number to make your next move:"
        printfn ""
        printfn "7|8|9"
        printfn "_____"
        printfn "4|5|6"
        printfn "_____"
        printfn "1|2|3"
        printfn ""
    else
        printfn "Please enter a number to make your next move:"
        printfn ""
        printfn "1|2|3"
        printfn "_____"
        printfn "4|5|6"
        printfn "_____"
        printfn "7|8|9"
        printfn ""
    0

let didTieHappen (gameState : char list) : bool = 
    if(not(gameState.[0] = ' ') && not(gameState.[1] = ' ') && not(gameState.[2] = ' ') && not(gameState.[3] = ' ') && not(gameState.[4] = ' ') && not(gameState.[5] = ' ') && not(gameState.[6] = ' ') && not(gameState.[7] = ' ') && not(gameState.[8] = ' ')) then
        true
    else
        false

let didSomeoneWin (gameState, playerCharacter : char) : bool =
    match gameState with
    | [a;b;c;_;_;_;_;_;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter) -> true
    | [_;_;_;a;b;c;_;_;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter) -> true
    | [_;_;_;_;_;_;a;b;c] when (a = playerCharacter && b = playerCharacter && c = playerCharacter) -> true
    | [a;_;_;b;_;_;c;_;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter) -> true
    | [_;a;_;_;b;_;_;c;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter) -> true
    | [_;_;a;_;_;b;_;_;c] when (a = playerCharacter && b = playerCharacter && c = playerCharacter) -> true
    | [a;_;_;_;b;_;_;_;c] when (a = playerCharacter && b = playerCharacter && c = playerCharacter) -> true
    | [_;_;a;_;b;_;c;_;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter) -> true
    | _ -> false

let isGameOver (gameState : char list, humanCharacter : char,computerCharacter : char) : bool = 
    if (didTieHappen(gameState) || didSomeoneWin(gameState, humanCharacter) || didSomeoneWin(gameState, computerCharacter)) then
        true
    else
        false

let rec moveInput (isBoardTopHeavy : bool) : int = 
    let input = System.Console.ReadKey().KeyChar
    printfn ""
    if(isBoardTopHeavy) then
        if(input = '1') then
            1
        elif(input = '2') then
            2
        elif(input = '3') then
            3
        elif(input = '4') then
            4
        elif(input = '5') then
            5
        elif(input = '6') then
            6
        elif(input = '7') then
            7
        elif(input = '8') then
            8
        elif(input = '9') then
            9
        else
            printfn "Not a legal move.  Please input a new move:"
            moveInput(isBoardTopHeavy)
    else
        if(input = '1') then
            7
        elif(input = '2') then
            8
        elif(input = '3') then
            9
        elif(input = '4') then
            4
        elif(input = '5') then
            5
        elif(input = '6') then
            6
        elif(input = '7') then
            1
        elif(input = '8') then
            2
        elif(input = '9') then
            3
        else
            printfn "Not a legal move.  Please input a new move:"
            moveInput(isBoardTopHeavy)

let rec humanMove (moveNum : int, gameState : char list, humanCharacter : char, isBoardTopHeavy : bool) : char list =
    if (moveNum = 1 || moveNum = 2 ||moveNum = 3 ||moveNum = 4 ||moveNum = 5 ||moveNum = 6 ||moveNum = 7 ||moveNum = 8 ||moveNum = 9) && gameState.[moveNum - 1] = ' '  then
        makeMove(gameState, humanCharacter, moveNum)
    else
        printfn "Not a legal move.  Please input a new move:"
        humanMove (moveInput(isBoardTopHeavy), gameState, humanCharacter, isBoardTopHeavy)

let checkHorizontalWins (userCharacter: char, gameState : char list) : int = 
    let mutable returnNum = -2
    for i in 0 .. 3 .. 8 do
        if(gameState.[i] = ' ' && gameState.[i+1] = userCharacter && gameState.[i+2] = userCharacter) then
            returnNum <- i
        elif(gameState.[i] = userCharacter && gameState.[i+1] = ' ' && gameState.[i+2] = userCharacter) then
            returnNum <- i + 1
        elif(gameState.[i] = userCharacter && gameState.[i+1] = userCharacter && gameState.[i+2] = ' ') then
            returnNum <- i + 2
    returnNum

let checkVerticalWins (userCharacter: char, gameState : char list) : int =
    let mutable returnNum = -2
    for j in 0 .. 2 do
        if(gameState.[j] = ' ' && gameState.[j+3] = userCharacter && gameState.[j+6] = userCharacter) then
            returnNum <- j
        elif(gameState.[j] = userCharacter && gameState.[j+3] = ' ' && gameState.[j+6] = userCharacter) then
            returnNum <- j + 3
        elif(gameState.[j] = userCharacter && gameState.[j+3] = userCharacter && gameState.[j+6] = ' ') then
            returnNum <- j + 6
    returnNum

let checkDiagonalWins (userCharacter: char, gameState : char list) : int =
    let mutable returnNum = -2
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
    returnNum

let winGameOrBlockWin (userCharacter: char, gameState : char list) : int = 
    let mutable returnNum = -2

    returnNum <- checkHorizontalWins (userCharacter, gameState)
    if(returnNum = -2) then
        returnNum <- checkVerticalWins (userCharacter, gameState)
        if(returnNum = -2) then
            returnNum <- checkDiagonalWins (userCharacter, gameState)
    returnNum + 1

let chooseCorner (gameState : char list) : int = 
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

let chooseSide (gameState : char list) : int = 
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

let chooseCornerInBetween (huMove : int, gameState : char list, firstHumanMove : int) : int=
    if((firstHumanMove = cornerSquares.[0] && huMove = cornerSquares.[3]) || (firstHumanMove = cornerSquares.[3] && huMove = cornerSquares.[0]) || (firstHumanMove = cornerSquares.[1] && huMove = cornerSquares.[2]) || (firstHumanMove = cornerSquares.[2] && huMove = cornerSquares.[1])) then
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

let isFirstComputerTurn (gameState : char list, humanCharacter : char) = 
    match gameState with
    | [humanCharacter;' ';' ';' ';' ';' ';' ';' ';' '] -> true
    | [' ';humanCharacter;' ';' ';' ';' ';' ';' ';' '] -> true
    | [' ';' ';humanCharacter;' ';' ';' ';' ';' ';' '] -> true
    | [' ';' ';' ';humanCharacter;' ';' ';' ';' ';' '] -> true
    | [' ';' ';' ';' ';humanCharacter;' ';' ';' ';' '] -> true
    | [' ';' ';' ';' ';' ';humanCharacter;' ';' ';' '] -> true
    | [' ';' ';' ';' ';' ';' ';humanCharacter;' ';' '] -> true
    | [' ';' ';' ';' ';' ';' ';' ';humanCharacter;' '] -> true
    | [' ';' ';' ';' ';' ';' ';' ';' ';humanCharacter] -> true
    | _ -> false

let respondToFirstMoveMiddle (gameState : char list, humanMoveSpot : int, humanCharacter : char) : int = 
    if(humanMoveSpot = middleSquare && isFirstComputerTurn(gameState, humanCharacter)) then
        cornerSquares.[0]
    else 
        -1

let respondToFirstMoveCorner (gameState : char list, humanMoveSpot : int, humanCharacter : char) : int = 
    if((humanMoveSpot = cornerSquares.[0] || humanMoveSpot = cornerSquares.[1] || humanMoveSpot = cornerSquares.[2] || humanMoveSpot = cornerSquares.[3]) && isFirstComputerTurn(gameState, humanCharacter)) then
        middleSquare
    else
        -1

let respondToFirstMoveSide (gameState : char list, humanCharacter : char) : int = 
    if(isFirstComputerTurn(gameState,humanCharacter)) then
        middleSquare
    else
        -1

let isFirstMove (gamestate : char list) : bool =
    if(gamestate = [' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']) then
        true
    else false

let makeFirstMove (gamestate) : int =
    if(isFirstMove(gamestate)) then
        1
    else
        -1

let respondToFirstMove (gameState : char list, humanMoveSpot : int, humanCharacter : char) : int =
    let mutable returnMove = -1

    if(returnMove = -1) then
        returnMove <- respondToFirstMoveMiddle(gameState, humanMoveSpot, humanCharacter)
        if(returnMove = -1) then
            returnMove <- respondToFirstMoveCorner(gameState, humanMoveSpot, humanCharacter)
            if(returnMove = -1) then
                returnMove <- respondToFirstMoveSide(gameState, humanCharacter)
    returnMove


(*It looks like the two functions for anything after first move can be combined*)
let respondToMiddleStrategy (gameState : char list, firstHumanMove : int, humanCharacter : char, computerCharacter : char) : int = 
    let mutable returnMove = -1
    if(firstHumanMove = middleSquare) then
        returnMove <- winGameOrBlockWin (computerCharacter, gameState)
        if(returnMove = -1) then
            returnMove <- winGameOrBlockWin (humanCharacter, gameState)
        if(returnMove = -1) then
            returnMove <- chooseCorner (gameState)
            if(returnMove = -1) then
                returnMove <- chooseSide (gameState)
    returnMove

let respondToSideOrCornerStrategy (gameState : char list, humanMoveSpot : int, firstHumanMove : int, humanCharacter : char, computerCharacter : char) : int = 
    let mutable returnMove = -1
    returnMove <- winGameOrBlockWin (computerCharacter, gameState)
    if(returnMove = -1) then
        returnMove <- winGameOrBlockWin (humanCharacter, gameState)
        if(returnMove = -1) then
            returnMove <- chooseCornerInBetween (humanMoveSpot, gameState, firstHumanMove)
            if(returnMove = -1) then
                returnMove <- chooseCorner (gameState)
                if(returnMove = -1) then
                    returnMove <- chooseSide (gameState)
    returnMove

let computerMove (gameState : char list, humanMoveSpot : int, firstHumanMove : int, humanCharacter : char, computerCharacter : char) : char list =
    printfn "Computer move..."
    System.Threading.Thread.Sleep(1000)
    let mutable computerMove = -1

    computerMove <- makeFirstMove(gameState)
    if(computerMove = -1) then
        computerMove <- respondToFirstMove (gameState, humanMoveSpot, humanCharacter)
        if(computerMove = -1) then
            computerMove <- respondToMiddleStrategy (gameState, firstHumanMove, humanCharacter, computerCharacter)
            if(computerMove = -1) then
                computerMove <- respondToSideOrCornerStrategy (gameState, humanMoveSpot, firstHumanMove, humanCharacter, computerCharacter)

    makeMove(gameState,computerCharacter,computerMove)

let rec playGame3X3 (gameState : char list, turn : int, humanMoveNum : int, firstMove : int, humanCharacter : char, computerCharacter : char, doesComputerGoFirst : bool, isBoardTopHeavy : bool) : char list =
        displayBoardState (gameState)

        if (turn % 2 = 1 && doesComputerGoFirst = false) || (turn % 2 = 0 && doesComputerGoFirst = true) then
            let x = askForInput (isBoardTopHeavy)
            let input = moveInput(isBoardTopHeavy)
            let newGameState = humanMove (input, gameState, humanCharacter, isBoardTopHeavy)

            if turn = 1 then
                playGame3X3 (newGameState, turn + 1, input, input, humanCharacter, computerCharacter,doesComputerGoFirst, isBoardTopHeavy)
            elif isGameOver (newGameState,humanCharacter, computerCharacter) then
                newGameState
            else
                playGame3X3 (newGameState, turn + 1, input, firstMove, humanCharacter, computerCharacter,doesComputerGoFirst, isBoardTopHeavy)
        else
            let newGameState = computerMove (gameState, humanMoveNum, firstMove,humanCharacter,computerCharacter)
            if isGameOver (newGameState, humanCharacter, computerCharacter) then
                newGameState
            else
                playGame3X3 (newGameState, turn + 1, humanMoveNum, firstMove, humanCharacter, computerCharacter,doesComputerGoFirst,isBoardTopHeavy)