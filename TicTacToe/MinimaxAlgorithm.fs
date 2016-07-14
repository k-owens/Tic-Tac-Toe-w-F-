module MinimaxAlgorithm
open GameBoard
open Game


let rec minimaxAlgorithm (gameBoard : Board, isPlayer, askingPlayer : Player, opposingPlayer : Player) = 
    if(didPlayer1Win (gameBoard.CurrentBoard, askingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
        (0,1)
    elif(didPlayer2Win (gameBoard.CurrentBoard, opposingPlayer.PlayerCharacter, gameBoard.BoardSize)) then
        (0,-1)
    elif(didTieHappen gameBoard.CurrentBoard) then
        (0,0)
    else
        if isPlayer then
            let mutable maximum = -1000
            let mutable resultArray = [||]

            for index in 0 .. (gameBoard.BoardSize*gameBoard.BoardSize - 1) do
                if(gameBoard.CurrentBoard.[index] = None) then
                    let newGameBoard = {BoardSize = gameBoard.BoardSize; CurrentBoard = makeMove(gameBoard,index,askingPlayer.PlayerCharacter); TurnNumber = gameBoard.TurnNumber+1; IsInverted = gameBoard.IsInverted}
                    let result = minimaxAlgorithm(newGameBoard, false, askingPlayer,opposingPlayer)
                    if maximum < snd result then
                        maximum <- snd result
                        resultArray <- [|index;maximum|]
            (resultArray.[0], resultArray.[1])
        else
            let mutable minimum = 1000
            let mutable resultArray = [||]

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
    fst result