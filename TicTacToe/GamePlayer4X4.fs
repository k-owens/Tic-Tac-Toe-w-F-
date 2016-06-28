module GamePlayer4X4

let middleSquares = [6;7;10;11]
let cornerSquares = [1;4;13;16]
let sideSquares = [2;3;5;8;9;12;14;15]

let makeMove4X4 (gameState : char list, player : char, place : int) : char list = 
    if(gameState.[place-1] = ' ') then
        if (place = 1) then
            let newGameState = [player;gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8];gameState.[9];gameState.[10];gameState.[11];gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 2) then
            let newGameState = [gameState.[0];player;gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8];gameState.[9];gameState.[10];gameState.[11];gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 3) then
            let newGameState = [gameState.[0];gameState.[1];player;gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8];gameState.[9];gameState.[10];gameState.[11];gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 4) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];player;gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8];gameState.[9];gameState.[10];gameState.[11];gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 5) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];player;gameState.[5];gameState.[6];gameState.[7];gameState.[8];gameState.[9];gameState.[10];gameState.[11];gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 6) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];player;gameState.[6];gameState.[7];gameState.[8];gameState.[9];gameState.[10];gameState.[11];gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 7) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];player;gameState.[7];gameState.[8];gameState.[9];gameState.[10];gameState.[11];gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 8) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];player;gameState.[8];gameState.[9];gameState.[10];gameState.[11];gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 9) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];player;gameState.[9];gameState.[10];gameState.[11];gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 10) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8];player;gameState.[10];gameState.[11];gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 11) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8];gameState.[9];player;gameState.[11];gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 12) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8];gameState.[9];gameState.[10];player;gameState.[12];gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 13) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8];gameState.[9];gameState.[10];gameState.[11];player;gameState.[13];gameState.[14];gameState.[15]]
            newGameState
        elif (place = 14) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8];gameState.[9];gameState.[10];gameState.[11];gameState.[12];player;gameState.[14];gameState.[15]]
            newGameState
        elif (place = 15) then
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8];gameState.[9];gameState.[10];gameState.[11];gameState.[12];gameState.[13];player;gameState.[15]]
            newGameState
        else
            let newGameState = [gameState.[0];gameState.[1];gameState.[2];gameState.[3];gameState.[4];gameState.[5];gameState.[6];gameState.[7];gameState.[8];gameState.[9];gameState.[10];gameState.[11];gameState.[12];gameState.[13];gameState.[14];player]
            newGameState
    else
    gameState

let displayBoardState4X4 (gameState : char list) =
        printfn ""
        printfn "Current board:"
        printfn "%c|%c|%c|%c" gameState.[12] gameState.[13] gameState.[14] gameState.[15]
        printfn "_______"
        printfn "%c|%c|%c|%c" gameState.[8] gameState.[9] gameState.[10] gameState.[11]
        printfn "_______"
        printfn "%c|%c|%c|%c" gameState.[4] gameState.[5] gameState.[6] gameState.[7]
        printfn "_______"
        printfn "%c|%c|%c|%c" gameState.[0] gameState.[1] gameState.[2] gameState.[3]

let askForInput4X4 (isBoardTopHeavy : bool) =
    if(isBoardTopHeavy) then
        printfn "Please enter a number to make your next move:"
        printfn ""
        printfn "1|2|3|4"
        printfn "_______"
        printfn "Q|W|E|R"
        printfn "_______"
        printfn "A|S|D|F"
        printfn "_______"
        printfn "Z|X|C|V"
        printfn ""
    else
        printfn "Please enter a number to make your next move:"
        printfn ""
        printfn "Z|X|C|V"
        printfn "_______"
        printfn "A|S|D|F"
        printfn "_______"
        printfn "Q|W|E|R"
        printfn "_______"
        printfn "1|2|3|4"
        printfn ""

let didTieHappen4X4 (gameState : char list) : bool = 
    if(not(gameState.[0] = ' ') && not(gameState.[1] = ' ') && not(gameState.[2] = ' ') && not(gameState.[3] = ' ') && not(gameState.[4] = ' ') && not(gameState.[5] = ' ') && not(gameState.[6] = ' ') && not(gameState.[7] = ' ') && not(gameState.[8] = ' ') && not(gameState.[9] = ' ') && not(gameState.[10] = ' ') && not(gameState.[11] = ' ') && not(gameState.[12] = ' ') && not(gameState.[13] = ' ') && not(gameState.[14] = ' ') && not(gameState.[15] = ' ')) then
        true
    else
        false

let didSomeoneWin4X4 (gameState, playerCharacter : char) : bool =
    match gameState with
    | [a;b;c;d;_;_;_;_;_;_;_;_;_;_;_;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter && d = playerCharacter) -> true
    | [_;_;_;_;a;b;c;d;_;_;_;_;_;_;_;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter && d = playerCharacter) -> true
    | [_;_;_;_;_;_;_;_;a;b;c;d;_;_;_;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter && d = playerCharacter) -> true
    | [_;_;_;_;_;_;_;_;_;_;_;_;a;b;c;d] when (a = playerCharacter && b = playerCharacter && c = playerCharacter && d = playerCharacter) -> true
    | [a;_;_;_;b;_;_;_;c;_;_;_;d;_;_;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter && d = playerCharacter) -> true
    | [_;a;_;_;_;b;_;_;_;c;_;_;_;d;_;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter && d = playerCharacter) -> true
    | [_;_;a;_;_;_;b;_;_;_;c;_;_;_;d;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter && d = playerCharacter) -> true
    | [_;_;_;a;_;_;b;_;_;_;_;c;_;_;_;d] when (a = playerCharacter && b = playerCharacter && c = playerCharacter && d = playerCharacter) -> true
    | [a;_;_;_;_;b;_;_;_;_;c;_;_;_;_;d] when (a = playerCharacter && b = playerCharacter && c = playerCharacter && d = playerCharacter) -> true
    | [_;_;_;a;_;_;b;_;_;c;_;_;d;_;_;_] when (a = playerCharacter && b = playerCharacter && c = playerCharacter && d = playerCharacter) -> true
    | _ -> false

let isGameOver4X4 (gameState : char list, humanCharacter : char,computerCharacter : char) : bool = 
    if (didTieHappen4X4(gameState) || didSomeoneWin4X4(gameState, humanCharacter) || didSomeoneWin4X4(gameState, computerCharacter)) then
        true
    else
        false

let rec moveInput4X4 (isBoardTopHeavy : bool) : int = 
    let input = System.Console.ReadKey().KeyChar
    printfn ""
    if(isBoardTopHeavy) then
        if(input = '1') then
            13
        elif(input = '2') then
            14
        elif(input = '3') then
            15
        elif(input = '4') then
            16
        elif(input = 'Q' || input = 'q') then
            9
        elif(input = 'W' || input = 'w') then
            10
        elif(input = 'E' || input = 'e') then
            11
        elif(input = 'R' || input = 'r') then
            12
        elif(input = 'A' || input = 'a') then
            5
        elif(input = 'S' || input = 's') then
            6
        elif(input = 'D' || input = 'D') then
            7
        elif(input = 'F' || input = 'f') then
            8
        elif(input = 'Z' || input = 'z') then
            1
        elif(input = 'X' || input = 'x') then
            2
        elif(input = 'C' || input = 'c') then
            3
        elif(input = 'V' || input = 'v') then
            4
        else
            printfn "Not a legal move.  Please input a new move:"
            moveInput4X4(isBoardTopHeavy)
    else
        if(input = '1') then
            1
        elif(input = '2') then
            2
        elif(input = '3') then
            3
        elif(input = '4') then
            4
        elif(input = 'Q' || input = 'q') then
            5
        elif(input = 'W' || input = 'w') then
            6
        elif(input = 'E' || input = 'e') then
            7
        elif(input = 'R' || input = 'r') then
            8
        elif(input = 'A' || input = 'a') then
            9
        elif(input = 'S' || input = 's') then
            10
        elif(input = 'D' || input = 'd') then
            11
        elif(input = 'F' || input = 'f') then
            12
        elif(input = 'Z' || input = 'z') then
            13
        elif(input = 'X' || input = 'x') then
            14
        elif(input = 'C' || input = 'c') then
            15
        elif(input = 'V' || input = 'v') then
            16
        else
            printfn "Not a legal move.  Please input a new move:"
            moveInput4X4(isBoardTopHeavy)

let rec humanMove4X4 (moveNum : int, gameState : char list, humanCharacter : char, isBoardTopHeavy : bool) : char list =
    if (moveNum = 1 || moveNum = 2 ||moveNum = 3 ||moveNum = 4 ||moveNum = 5 ||moveNum = 6 ||moveNum = 7 ||moveNum = 8 ||moveNum = 9 ||moveNum = 10 ||moveNum = 11 ||moveNum = 12 ||moveNum = 13 ||moveNum = 14 ||moveNum = 15 ||moveNum = 16) && gameState.[moveNum - 1] = ' '  then
        makeMove4X4(gameState, humanCharacter, moveNum)
    else
        (*Dependencies*)
        printfn "Not a legal move.  Please input a new move:"
        humanMove4X4 (moveInput4X4(isBoardTopHeavy), gameState, humanCharacter, isBoardTopHeavy)

let chooseMiddle4X4 (gameState : char list) : int =
    if (gameState.[middleSquares.[0] - 1] = ' ') then
        middleSquares.[0]
    elif (gameState.[middleSquares.[1] - 1] = ' ') then
        middleSquares.[1]
    elif (gameState.[middleSquares.[2] - 1] = ' ') then
        middleSquares.[2]
    elif (gameState.[middleSquares.[3] - 1] = ' ') then
        middleSquares.[3]
    else
        -1
        
let chooseCorner4X4 (gameState : char list) : int =
    if (gameState.[cornerSquares.[0] - 1] = ' ') then
        cornerSquares.[0]
    elif (gameState.[cornerSquares.[1] - 1] = ' ') then
        cornerSquares.[1]
    elif (gameState.[cornerSquares.[2] - 1] = ' ') then
        cornerSquares.[2]
    elif (gameState.[cornerSquares.[3] - 1] = ' ') then
        cornerSquares.[3]
    else
        -1

let chooseSide4X4 (gameState : char list) : int = 
    if (gameState.[sideSquares.[0] - 1] = ' ') then
        sideSquares.[0]
    elif (gameState.[sideSquares.[1] - 1] = ' ') then
        sideSquares.[1]
    elif (gameState.[sideSquares.[2] - 1] = ' ') then
        sideSquares.[2]
    elif (gameState.[sideSquares.[3] - 1] = ' ') then
        sideSquares.[3]
    elif (gameState.[sideSquares.[4] - 1] = ' ') then
        sideSquares.[4]
    elif (gameState.[sideSquares.[5] - 1] = ' ') then
        sideSquares.[5]
    elif (gameState.[sideSquares.[6] - 1] = ' ') then
        sideSquares.[6]
    elif (gameState.[sideSquares.[7] - 1] = ' ') then
        sideSquares.[7]
    else
        -1

let checkHorizontalWins4X4 (gameState : char list, player : char) : int =
    match gameState with
    | [' ';a;b;c;_;_;_;_;_;_;_;_;_;_;_;_] when (a = player && b = player && c = player) -> 1
    | [a;' ';b;c;_;_;_;_;_;_;_;_;_;_;_;_] when (a = player && b = player && c = player) -> 2
    | [a;b;' ';c;_;_;_;_;_;_;_;_;_;_;_;_] when (a = player && b = player && c = player) -> 3
    | [a;b;c;' ';_;_;_;_;_;_;_;_;_;_;_;_] when (a = player && b = player && c = player) -> 4
    | [_;_;_;_;' ';a;b;c;_;_;_;_;_;_;_;_] when (a = player && b = player && c = player) -> 5
    | [_;_;_;_;a;' ';b;c;_;_;_;_;_;_;_;_] when (a = player && b = player && c = player) -> 6
    | [_;_;_;_;a;b;' ';c;_;_;_;_;_;_;_;_] when (a = player && b = player && c = player) -> 7
    | [_;_;_;_;a;b;c;' ';_;_;_;_;_;_;_;_] when (a = player && b = player && c = player) -> 8
    | [_;_;_;_;_;_;_;_;' ';a;b;c;_;_;_;_] when (a = player && b = player && c = player) -> 9
    | [_;_;_;_;_;_;_;_;a;' ';b;c;_;_;_;_] when (a = player && b = player && c = player) -> 10
    | [_;_;_;_;_;_;_;_;a;b;' ';c;_;_;_;_] when (a = player && b = player && c = player) -> 11
    | [_;_;_;_;_;_;_;_;a;b;c;' ';_;_;_;_] when (a = player && b = player && c = player) -> 12
    | [_;_;_;_;_;_;_;_;_;_;_;_;' ';a;b;c] when (a = player && b = player && c = player) -> 13
    | [_;_;_;_;_;_;_;_;_;_;_;_;a;' ';b;c] when (a = player && b = player && c = player) -> 14
    | [_;_;_;_;_;_;_;_;_;_;_;_;a;b;' ';c] when (a = player && b = player && c = player) -> 15
    | [_;_;_;_;_;_;_;_;_;_;_;_;a;b;c;' '] when (a = player && b = player && c = player) -> 16
    | _ -> -1

let checkVerticalWins4X4 (gameState : char list, player : char) : int =
    match gameState with
    | [' ';_;_;_;a;_;_;_;b;_;_;_;c;_;_;_] when (a = player && b = player && c = player) -> 1
    | [_;' ';_;_;_;a;_;_;_;b;_;_;_;c;_;_] when (a = player && b = player && c = player) -> 2
    | [_;_;' ';_;_;_;a;_;_;_;b;_;_;_;c;_] when (a = player && b = player && c = player) -> 3
    | [_;_;_;' ';_;_;_;a;_;_;_;b;_;_;_;c] when (a = player && b = player && c = player) -> 4
    | [a;_;_;_;' ';_;_;_;b;_;_;_;c;_;_;_] when (a = player && b = player && c = player) -> 5
    | [_;a;_;_;_;' ';_;_;_;b;_;_;_;c;_;_] when (a = player && b = player && c = player) -> 6
    | [_;_;a;_;_;_;' ';_;_;_;b;_;_;_;c;_] when (a = player && b = player && c = player) -> 7
    | [_;_;_;a;_;_;_;' ';_;_;_;b;_;_;_;c] when (a = player && b = player && c = player) -> 8
    | [a;_;_;_;b;_;_;_;' ';_;_;_;c;_;_;_] when (a = player && b = player && c = player) -> 9
    | [_;a;_;_;_;b;_;_;_;' ';_;_;_;c;_;_] when (a = player && b = player && c = player) -> 10
    | [_;_;a;_;_;_;b;_;_;_;' ';_;_;_;c;_] when (a = player && b = player && c = player) -> 11
    | [_;_;_;a;_;_;_;b;_;_;_;' ';_;_;_;c] when (a = player && b = player && c = player) -> 12
    | [a;_;_;_;b;_;_;_;c;_;_;_;' ';_;_;_] when (a = player && b = player && c = player) -> 13
    | [_;a;_;_;_;b;_;_;_;c;_;_;_;' ';_;_] when (a = player && b = player && c = player) -> 14
    | [_;_;a;_;_;_;b;_;_;_;c;_;_;_;' ';_] when (a = player && b = player && c = player) -> 15
    | [_;_;_;a;_;_;_;b;_;_;_;c;_;_;_;' '] when (a = player && b = player && c = player) -> 16
    | _ -> -1

let checkDiagonalWins4X4 (gameState : char list, player : char) : int =
    match gameState with
    | [' ';_;_;_;_;a;_;_;_;_;b;_;_;_;_;c] when (a = player && b = player && c = player) -> 1
    | [a;_;_;_;_;' ';_;_;_;_;b;_;_;_;_;c] when (a = player && b = player && c = player) -> 6
    | [a;_;_;_;_;b;_;_;_;_;' ';_;_;_;_;c] when (a = player && b = player && c = player) -> 11
    | [a;_;_;_;_;b;_;_;_;_;c;_;_;_;_;' '] when (a = player && b = player && c = player) -> 16
    | [_;_;_;' ';_;_;a;_;_;b;_;_;c;_;_;_] when (a = player && b = player && c = player) -> 4
    | [_;_;_;a;_;_;' ';_;_;b;_;_;c;_;_;_] when (a = player && b = player && c = player) -> 7
    | [_;_;_;a;_;_;b;_;_;' ';_;_;c;_;_;_] when (a = player && b = player && c = player) -> 10
    | [_;_;_;a;_;_;b;_;_;c;_;_;' ';_;_;_] when (a = player && b = player && c = player) -> 13
    | _ -> -1


let areTwoSelectedByHuman4X4 (gameState : char list, player: char, check1, check2, check3, check4) : bool = 
    let mutable x = 0
    if(gameState.[check1 - 1] = player) then
        x <- x + 1
    if(gameState.[check2 - 1] = player) then
        x <- x + 1
    if(gameState.[check3 - 1] = player) then
        x <- x + 1
    if(gameState.[check4 - 1] = player) then
        x <- x + 1

    if(x > 1) then
        true
    else
        false

let selectBlank4X4 (gameState : char list, computerCharacter: char, check1, check2, check3, check4) : int =
    if(gameState.[check1 - 1] = ' ' && not(gameState.[check2 - 1] = computerCharacter) && not(gameState.[check3 - 1] = computerCharacter) && not(gameState.[check4 - 1] = computerCharacter)) then
        check1
    elif(gameState.[check2 - 1] = ' ' && not(gameState.[check1 - 1] = computerCharacter) && not(gameState.[check3 - 1] = computerCharacter) && not(gameState.[check4 - 1] = computerCharacter)) then
        check2
    elif(gameState.[check3 - 1] = ' ' && not(gameState.[check1 - 1] = computerCharacter) && not(gameState.[check2 - 1] = computerCharacter) && not(gameState.[check4 - 1] = computerCharacter)) then
        check3
    elif(gameState.[check4 - 1] = ' ' && not(gameState.[check1 - 1] = computerCharacter) && not(gameState.[check2 - 1] = computerCharacter) && not(gameState.[check3 - 1] = computerCharacter)) then
        check4
    else
        -1

let blockHorizontalWin4X4 (gameState : char list, computerCharacter : char, player : char) : int = 
    let mutable returnNum = -1

    if(areTwoSelectedByHuman4X4(gameState,player,1,2,3,4)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,1,2,3,4)
    if(returnNum = -1 && areTwoSelectedByHuman4X4(gameState,player,5,6,7,8)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,5,6,7,8)
    if(returnNum = -1 && areTwoSelectedByHuman4X4(gameState,player,9,10,11,12)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,9,10,11,12)
    if(returnNum = -1 && areTwoSelectedByHuman4X4(gameState,player,13,14,15,16)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,13,14,15,16)
    returnNum

let blockVerticalWin4X4 (gameState : char list, computerCharacter : char, player : char) : int = 
    let mutable returnNum = -1

    if(areTwoSelectedByHuman4X4(gameState,player,1,5,9,13)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,1,5,9,13)
    if(returnNum = -1 && areTwoSelectedByHuman4X4(gameState,player,2,6,10,14)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,2,6,10,14)
    if(returnNum = -1 && areTwoSelectedByHuman4X4(gameState,player,3,7,11,15)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,3,7,11,15)
    if(returnNum = -1 && areTwoSelectedByHuman4X4(gameState,player,4,8,12,16)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,4,8,12,16)
    returnNum

let blockDiagonalWin4X4 (gameState : char list, computerCharacter : char, player : char) : int = 
    let mutable returnNum = -1

    if(areTwoSelectedByHuman4X4(gameState,player,1,6,11,16)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,1,6,11,16)
    if(returnNum = -1 && areTwoSelectedByHuman4X4(gameState,player,4,7,10,13)) then
        returnNum <- selectBlank4X4(gameState,computerCharacter,4,7,10,13)
    returnNum

let blockWin4X4 (gameState : char list, computerCharacter : char, humanPlayer) : int =
    let mutable returnNum = -1
    returnNum <- blockHorizontalWin4X4(gameState,computerCharacter,humanPlayer)
    if(returnNum = -1) then
        returnNum <- blockVerticalWin4X4(gameState,computerCharacter,humanPlayer)
        if(returnNum = -1) then
            returnNum <- blockDiagonalWin4X4(gameState,computerCharacter,humanPlayer)
    returnNum

let winGame4X4 () : int = 
    -1

let computerMove4X4 (gameState : char list, humanMoveSpot : int, firstHumanMove : int, humanCharacter : char, computerCharacter : char) : char list =
    printfn "Computer move..."
    System.Threading.Thread.Sleep(1000)
    let mutable computerMove = -1

    computerMove <- winGame4X4()
    if(computerMove = -1) then
        computerMove <- blockWin4X4(gameState, computerCharacter, humanCharacter)
        if(computerMove = -1) then
            computerMove <- chooseMiddle4X4(gameState)
            if(computerMove = -1) then
                computerMove <- chooseCorner4X4(gameState)
                if(computerMove = -1) then
                    computerMove <- chooseSide4X4(gameState)

    makeMove4X4(gameState,computerCharacter,computerMove)

let rec playGame4X4 (gameState : char list, turn : int, humanMoveNum : int, firstMove : int, humanCharacter : char, computerCharacter : char, doesComputerGoFirst : bool, isBoardTopHeavy : bool) : char list =
        displayBoardState4X4 (gameState)

        if (turn % 2 = 1 && doesComputerGoFirst = false) || (turn % 2 = 0 && doesComputerGoFirst = true) then
            askForInput4X4 (isBoardTopHeavy)
            let input = moveInput4X4(isBoardTopHeavy)
            let newGameState = humanMove4X4 (input, gameState, humanCharacter, isBoardTopHeavy)

            if turn = 1 then
                playGame4X4 (newGameState, turn + 1, input, input, humanCharacter, computerCharacter,doesComputerGoFirst, isBoardTopHeavy)
            elif isGameOver4X4 (newGameState,humanCharacter, computerCharacter) then
                newGameState
            else
                playGame4X4 (newGameState, turn + 1, input, firstMove, humanCharacter, computerCharacter,doesComputerGoFirst, isBoardTopHeavy)
        else
            let newGameState = computerMove4X4 (gameState, humanMoveNum, firstMove,humanCharacter,computerCharacter)
            if isGameOver4X4 (newGameState, humanCharacter, computerCharacter) then
                newGameState
            else
                playGame4X4 (newGameState, turn + 1, humanMoveNum, firstMove, humanCharacter, computerCharacter,doesComputerGoFirst,isBoardTopHeavy)