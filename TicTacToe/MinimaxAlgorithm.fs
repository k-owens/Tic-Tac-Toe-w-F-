module MinimaxAlgorithm
open GameBoard
open Game

let rec minimaxAlgorithm (game : Game, isComputerPlayer) =

    if(didPlayer1Win game.CurrentBoard game.ComputerCharacter game.BoardSize) then
        [0;1]
    elif(didPlayer2Win game.CurrentBoard game.HumanCharacter game.BoardSize) then
        [0;-1]
    elif(didTieHappen game.CurrentBoard) then
        [0;0]
    else
        if isComputerPlayer then
            let mutable maximum = -1000
            let mutable resultArray = [||]

            for index in 0 .. (game.BoardSize*game.BoardSize - 1) do
                if(game.CurrentBoard.[index] = ' ') then
                    let newGame = {BoardSize = game.BoardSize; CurrentBoard = makeMove(game,index,game.ComputerCharacter); HumanCharacter = game.HumanCharacter; ComputerCharacter = game.ComputerCharacter; DoesComputerGoFirst = game.DoesComputerGoFirst; TurnNumber = game.TurnNumber+1}
                    let result = minimaxAlgorithm(newGame, false)
                    if maximum < result.[1] then
                        maximum <- result.[1]    
                        resultArray <- [|index;maximum|]
            [resultArray.[0]; resultArray.[1]]
        else
            let mutable minimum = 1000
            let mutable resultArray = [||]

            for index in 0 .. (game.BoardSize*game.BoardSize - 1) do
                if(game.CurrentBoard.[index] = ' ') then
                    let newGame = {BoardSize = game.BoardSize; CurrentBoard = makeMove(game,index,game.HumanCharacter); HumanCharacter = game.HumanCharacter; ComputerCharacter = game.ComputerCharacter; DoesComputerGoFirst = game.DoesComputerGoFirst; TurnNumber = game.TurnNumber+1}
                    let result = minimaxAlgorithm(newGame, true)
                    if minimum > result.[1] then
                        minimum <- result.[1]    
                        resultArray <- [|index;minimum|]
            [resultArray.[0]; resultArray.[1]]