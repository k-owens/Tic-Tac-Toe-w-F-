module MinimaxAlgorithm
open GameBoard
open Game

let availableBoards (gameBoard : Board, player) =
    let moves = [for location in 0 .. (gameBoard.BoardSize*gameBoard.BoardSize - 1) -> if(gameBoard.CurrentBoard.[location] = None) then Some(location) else None] |> List.choose id
    [for location in 0 .. (List.length moves)-1 -> {BoardSize = gameBoard.BoardSize; CurrentBoard = makeMove(gameBoard,moves.[location],player.PlayerCharacter); TurnNumber = gameBoard.TurnNumber+1; IsInverted = gameBoard.IsInverted}]



let rec minimaxAlgorithm (gameBoard : Board, isPlayer, askingPlayer : Player, opposingPlayer : Player) =              
    if(didPlayer1Win (gameBoard.CurrentBoard, askingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
        1
    elif(didPlayer2Win (gameBoard.CurrentBoard, opposingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
        -1
    elif(didTieHappen gameBoard.CurrentBoard) then
        0
    elif isPlayer then
        let futureBoards = availableBoards(gameBoard, askingPlayer)
        let scores = List.init (List.length futureBoards) (fun i ->minimaxAlgorithm(futureBoards.[i], false, askingPlayer,opposingPlayer))
        let maxScore = List.max scores
        maxScore
    else
        let futureBoards = availableBoards(gameBoard, opposingPlayer)
        let scores = List.init (List.length futureBoards) (fun i ->minimaxAlgorithm(futureBoards.[i], true, askingPlayer,opposingPlayer))
        let minScore = List.min scores
        minScore

            
let minimaxMove (gameBoard : Board, askingPlayer : Player, opposingPlayer : Player) : int =
    let moves = possibleMoves(gameBoard)
    let futureBoards = availableBoards(gameBoard, askingPlayer)
    let results = List.init (List.length futureBoards) (fun i ->minimaxAlgorithm(futureBoards.[i], false, askingPlayer,opposingPlayer))
    let maxScore = List.max results
    let location = List.findIndex(fun elem -> elem = maxScore) results
    moves.[location]