module GameBoard

let startingBoard size = 
    [for i in 0 .. (size*size) -> ' ']


let isLegalMove move boardSize = 
    move >= 0 && move < boardSize*boardSize


let makeMove (gameState : char list, boardSize, moveIndex, player) =
    List.init (boardSize * boardSize) (fun i -> if moveIndex = i then player else gameState.[i])


let didTieHappen gameState =
    not(List.exists (fun elem -> elem = ' ') gameState)


let didWin (row: char list, player1) =
    List.forall (fun elem -> elem = player1) row


let didHorizontalWinHappen (gameState : char list, player1, boardSize) =
    let rows = List.init boardSize (fun elem -> List.init boardSize (fun i -> gameState.[i+(elem * boardSize)]))
    List.exists (fun elem -> didWin (elem, player1)) rows


let didVerticalWinHappen (gameState : char list, player1, boardSize) =
    let columns = List.init boardSize (fun elem -> List.init boardSize (fun i -> gameState.[(i * boardSize)+elem]))
    List.exists (fun elem -> didWin (elem, player1)) columns


let didDiagonalWinHappen (gameState : char list, player1, boardSize) =
    let diagonal1 = List.init boardSize (fun i -> gameState.[i+(boardSize*i)])
    let diagonal2 = List.init boardSize (fun i -> gameState.[(i*(boardSize-1)) + (boardSize - 1)])
    didWin (diagonal1, player1) || didWin (diagonal2, player1)


let didPlayer1Win gameState player1 boardSize =
    didHorizontalWinHappen (gameState, player1, boardSize)
    || didVerticalWinHappen (gameState, player1, boardSize)
    || didDiagonalWinHappen (gameState, player1, boardSize)


let didPlayer2Win gameState player2 boardSize =
    didHorizontalWinHappen (gameState, player2, boardSize)
    || didVerticalWinHappen (gameState, player2, boardSize)
    || didDiagonalWinHappen (gameState, player2, boardSize)


let didSomeoneWin gameState player1 player2 boardSize = 
    didPlayer1Win gameState player1 boardSize
    || didPlayer2Win gameState player1 boardSize


let isGameOver gameState player1 player2 boardSize = 
    didTieHappen gameState || didSomeoneWin gameState player1 player2 boardSize