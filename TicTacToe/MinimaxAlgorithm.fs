module MinimaxAlgorithm
open GameBoard
open Game


let rec minimaxAlgorithm (gameBoard : Board, isPlayer, askingPlayer : Player, opposingPlayer : Player) = 
    
    (*let rec alphaBetaPruning (alpha, beta, isAskingPlayer) =
        if(didPlayer1Win (gameBoard.CurrentBoard, askingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
            1
        elif(didPlayer2Win (gameBoard.CurrentBoard, opposingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
            -1
        elif(didTieHappen gameBoard.CurrentBoard) then
            0
        elif(isAskingPlayer)
            for(all moves) do
                //if not none
                score = alphaBetaPruning (alpha, beta, false)
                if(score >= beta) then
                    beta
                elif(score > alpha) then
                    alpha <- score
                    continue
                else
                    continue
            alpha
        else
            for(all moves) do
                //if not none
                score = alphaBetaPruning (alpha, beta, true)
                if(score <= alpha) then
                    alpha
                elif(score < beta) then
                    beta <- score
                    continue
                else
                    continue
            beta
    
    let moves = [for location in 0 .. (gameBoard.BoardSize*gameBoard.BoardSize - 1) -> if(gameBoard.CurrentBoard.[location] = None) then Some(location) else None ]
    let (scores : int list) = [for i in 0 .. (List.length moves) ->  alphaBetaPruning(-10,10,true)]
    let bestScore = List.max scores
    List.findIndex(fun elem -> elem = bestScore) moves // int option, needs to be int*)

             
    if(didPlayer1Win (gameBoard.CurrentBoard, askingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
        (0,1)
    elif(didPlayer2Win (gameBoard.CurrentBoard, opposingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
        (0,-1)
    elif(didTieHappen gameBoard.CurrentBoard) then
        (0,0)
    else
        if isPlayer then
            let mutable maximum = -1000 // mutable
            let mutable resultArray = [||] // mutable

            for index in 0 .. (gameBoard.BoardSize*gameBoard.BoardSize - 1) do
                if(gameBoard.CurrentBoard.[index] = None) then
                    let newGameBoard = {BoardSize = gameBoard.BoardSize; CurrentBoard = makeMove(gameBoard,index,askingPlayer.PlayerCharacter); TurnNumber = gameBoard.TurnNumber+1; IsInverted = gameBoard.IsInverted}
                    let result = minimaxAlgorithm(newGameBoard, false, askingPlayer,opposingPlayer)
                    if maximum < snd result then
                        maximum <- snd result
                        resultArray <- [|index;maximum|]
            (resultArray.[0], resultArray.[1])
        else
            let mutable minimum = 1000 // mutable
            let mutable resultArray = [||] //mutable

            for index in 0 .. (gameBoard.BoardSize*gameBoard.BoardSize - 1) do
                if(gameBoard.CurrentBoard.[index] = None) then
                    let newGameBoard = {BoardSize = gameBoard.BoardSize; CurrentBoard = makeMove(gameBoard,index,opposingPlayer.PlayerCharacter); TurnNumber = gameBoard.TurnNumber+1; IsInverted = gameBoard.IsInverted}
                    let result = minimaxAlgorithm(newGameBoard, true, askingPlayer,opposingPlayer)
                    if minimum > snd result then
                        minimum <- snd result    
                        resultArray <- [|index;minimum|]
            (resultArray.[0], resultArray.[1])
            
let minimaxStarter (gameBoard : Board, askingPlayer : Player, opposingPlayer : Player) =
    let result = minimaxAlgorithm(gameBoard, true, askingPlayer, opposingPlayer)
    fst result //result