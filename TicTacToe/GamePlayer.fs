module GamePlayer
open GameBoard
open MoveManager
open ConsoleTasks
open Game


let returnFunction () =
    0


let turn (currentGame : Game, enterMove, print,isInverted,player1 : Player, player2 : Player) =
    if(currentGame.TurnNumber % 2 = 1 ) then
        print(displayBoardOptions(currentGame.BoardSize,isInverted))
        makeMove(currentGame, moveTypeSelector (player1,moveInput, currentGame, enterMove, print,currentGame, player1.ComputerAlgorithm, player2), player1.PlayerCharacter)
    else
        print "Computer Move:"
        makeMove(currentGame, moveTypeSelector (player2,moveInput, currentGame, enterMove, print,currentGame, player2.ComputerAlgorithm, player1), player2.PlayerCharacter)


let rec playGame (currentGame, enterMove, print, isInverted, player1, player2) =
    print(displayBoard(currentGame.CurrentBoard,currentGame.BoardSize,isInverted))
    if not(isGameOver (currentGame,player1,player2)) then
        let newGameState = turn(currentGame, enterMove, print,isInverted,player1,player2)
        let updatedGame = {BoardSize = currentGame.BoardSize; CurrentBoard = newGameState; TurnNumber = currentGame.TurnNumber+1}
        playGame(updatedGame, enterMove, print,isInverted, player1, player2)
    else
        print ("The game is over.  The computer is still unbeaten.")
        0


let startNewGame (boardSize, humanCharacter, computerCharacter, enterMove, print,isInverted, player1, player2) =
    let x = {BoardSize = boardSize; CurrentBoard = startingBoard boardSize; TurnNumber = 1}
    playGame(x, enterMove, print,isInverted, player1, player2)