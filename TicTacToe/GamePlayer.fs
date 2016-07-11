module GamePlayer
open GameBoard
open MoveManager
open ConsoleTasks
open Game


let returnFunction () =
    0


let turn (currentGame : Game, enterMove, print,isInverted,algorithmSeed) =
    if((currentGame.TurnNumber % 2 = 1 && currentGame.DoesComputerGoFirst = false) || (currentGame.TurnNumber % 2 = 0 && currentGame.DoesComputerGoFirst = true)) then
        print(displayBoardOptions(currentGame.BoardSize,isInverted))
        makeMove(currentGame, humanMove (moveInput, currentGame, enterMove, print), currentGame.HumanCharacter)
    else
        print "Computer Move:"
        makeMove(currentGame, computerMove (currentGame, algorithmSeed), currentGame.ComputerCharacter)


let rec playGame (currentGame, enterMove, print, isInverted,algorithmSeed) =
    print(displayBoard(currentGame.CurrentBoard,currentGame.BoardSize,isInverted))
    if not(isGameOver (currentGame)) then
        let newGameState = turn(currentGame, enterMove, print,isInverted,algorithmSeed)
        let updatedGame = {BoardSize = currentGame.BoardSize; CurrentBoard = newGameState; HumanCharacter = currentGame.HumanCharacter; ComputerCharacter = currentGame.ComputerCharacter; DoesComputerGoFirst = currentGame.DoesComputerGoFirst; TurnNumber = currentGame.TurnNumber+1}
        playGame(updatedGame, enterMove, print,isInverted,algorithmSeed)
    else
        print ("The game is over.  The computer is still unbeaten.")
        0


let startNewGame (boardSize, doesComputerGoFirst, humanCharacter, computerCharacter, enterMove, print,isInverted,algorithmSeed) =
    let x = {BoardSize = boardSize; CurrentBoard = startingBoard boardSize; HumanCharacter = humanCharacter; ComputerCharacter = computerCharacter; DoesComputerGoFirst = doesComputerGoFirst; TurnNumber = 1}
    playGame(x, enterMove, print,isInverted,algorithmSeed)