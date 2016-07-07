module GamePlayer
open GameBoard
open MoveManager
open ConsoleTasks
open Game


let returnFunction () =
    0


let turn (currentGame : Game, enterMove, print) =
    print(displayBoard(currentGame.CurrentBoard,currentGame.BoardSize,false))
    if((currentGame.TurnNumber % 2 = 1 && currentGame.DoesComputerGoFirst = false) || (currentGame.TurnNumber % 2 = 0 && currentGame.DoesComputerGoFirst = true)) then
        print(displayBoardOptions(currentGame.BoardSize,false))
        makeMove(currentGame, humanMove (moveInput, currentGame, enterMove, print), currentGame.HumanCharacter)
    else
        print "Computer Move:"
        makeMove(currentGame, computerMove (currentGame.CurrentBoard, true), currentGame.ComputerCharacter)


let rec playGame (currentGame, enterMove, print) =
    let newGameState = turn(currentGame, enterMove, print)

    if not(isGameOver (currentGame)) then
        let updatedGame = {BoardSize = currentGame.BoardSize; CurrentBoard = newGameState; HumanCharacter = currentGame.HumanCharacter; ComputerCharacter = currentGame.ComputerCharacter; DoesComputerGoFirst = currentGame.DoesComputerGoFirst; TurnNumber = currentGame.TurnNumber+1}
        playGame(updatedGame, enterMove, print)
    else
        0


let startNewGame (boardSize, doesComputerGoFirst, humanCharacter, computerCharacter, enterMove, print) =
    let x = {BoardSize = boardSize; CurrentBoard = startingBoard boardSize; HumanCharacter = humanCharacter; ComputerCharacter = computerCharacter; DoesComputerGoFirst = doesComputerGoFirst; TurnNumber = 1}
    playGame(x, enterMove, print)