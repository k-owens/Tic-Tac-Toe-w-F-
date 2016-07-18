module MinimaxAlgorithm
open GameBoard
open Game

let availableBoards (gameBoard : Board, player) =
    let moves = [for location in 0 .. (gameBoard.BoardSize*gameBoard.BoardSize - 1) -> if(gameBoard.CurrentBoard.[location] = None) then Some(location) else None] |> List.choose id
    [for location in 0 .. (List.length moves)-1 -> {BoardSize = gameBoard.BoardSize; CurrentBoard = makeMove(gameBoard,moves.[location],player.PlayerCharacter); TurnNumber = gameBoard.TurnNumber+1; IsInverted = gameBoard.IsInverted}]



let rec minimaxAlgorithm (gameBoard : Board, isPlayer, askingPlayer : Player, opposingPlayer : Player) =              
    if(didPlayer1Win (gameBoard.CurrentBoard, askingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
        (0,1)
    elif(didPlayer2Win (gameBoard.CurrentBoard, opposingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
        (0,-1)
    elif(didTieHappen gameBoard.CurrentBoard) then
        (0,0)
    elif isPlayer then
        let moves = possibleMoves(gameBoard)
        let futureBoards = availableBoards(gameBoard, askingPlayer)
        let results = [for i in 0 .. (List.length futureBoards)-1 ->minimaxAlgorithm(futureBoards.[i], false, askingPlayer,opposingPlayer)]
        let scores = [for i in 0 .. (List.length results)-1 -> snd results.[i]]
        let maxScore = List.max scores
        let location = List.findIndex(fun elem -> elem = maxScore) scores
        (moves.[location],maxScore)
    else
        let moves = possibleMoves(gameBoard)
        let futureBoards = availableBoards(gameBoard, opposingPlayer)
        let results = [for i in 0 .. (List.length futureBoards)-1 ->minimaxAlgorithm(futureBoards.[i], true, askingPlayer,opposingPlayer)]
        let scores = [for i in 0 .. (List.length results)-1 -> snd results.[i]]
        let minScore = List.min scores
        let location = List.findIndex(fun elem -> elem = minScore) scores
        (moves.[location],minScore)

            
let minimaxStarter (gameBoard : Board, askingPlayer : Player, opposingPlayer : Player) : int =
    let result = minimaxAlgorithm(gameBoard,true,askingPlayer,opposingPlayer)
    fst result