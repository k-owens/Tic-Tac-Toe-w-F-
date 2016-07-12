module MinimaxAlgorithm
open GameBoard
open Game

let rec minimaxAlgorithm (game : Game, isPlayer, askingPlayer , opposingPlayer) =

    if(didPlayer1Win game.CurrentBoard askingPlayer.PlayerCharacter game.BoardSize) then
        [0;1]
    elif(didPlayer2Win game.CurrentBoard opposingPlayer.PlayerCharacter game.BoardSize) then
        [0;-1]
    elif(didTieHappen game.CurrentBoard) then
        [0;0]
    else
        if isPlayer then
            let mutable maximum = -1000
            let mutable resultArray = [||]

            for index in 0 .. (game.BoardSize*game.BoardSize - 1) do
                if(game.CurrentBoard.[index] = None) then
                    let newGame = {BoardSize = game.BoardSize; CurrentBoard = makeMove(game,index,askingPlayer.PlayerCharacter); TurnNumber = game.TurnNumber+1}
                    let result = minimaxAlgorithm(newGame, false, askingPlayer , opposingPlayer)
                    if maximum < result.[1] then
                        maximum <- result.[1]    
                        resultArray <- [|index;maximum|]
            [resultArray.[0]; resultArray.[1]]
        else
            let mutable minimum = 1000
            let mutable resultArray = [||]

            for index in 0 .. (game.BoardSize*game.BoardSize - 1) do
                if(game.CurrentBoard.[index] = None) then
                    let newGame = {BoardSize = game.BoardSize; CurrentBoard = makeMove(game,index,opposingPlayer.PlayerCharacter); TurnNumber = game.TurnNumber+1}
                    let result = minimaxAlgorithm(newGame, true,  askingPlayer , opposingPlayer)
                    if minimum > result.[1] then
                        minimum <- result.[1]    
                        resultArray <- [|index;minimum|]
            [resultArray.[0]; resultArray.[1]]