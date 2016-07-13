module GamePlayer
open GameBoard
open MoveManager
open ConsoleTasks
open UserSelection
open Game


let returnFunction () =
    0


let playerTurn (game : Game, playerNumber) =
        game.Functions.PrintFunction ("Player " + playerNumber.ToString() + " Move:")
        game.Functions.PrintFunction(displayBoardOptions(game))
        makeMove(game.GameBoard, moveTypeSelector (game, playerNumber,moveInput), game.Players.[playerNumber-1].PlayerCharacter)


let turn (game : Game) =
    if(game.GameBoard.TurnNumber % 2 = 1 ) then
        playerTurn(game,1)
    else
        playerTurn(game,2)


let rec playGame (game : Game) =
    game.Functions.PrintFunction(displayBoard(game))
    if not(isGameOver (game)) then
        let newBoardState = turn(game)
        let updatedBoard = {BoardSize = game.GameBoard.BoardSize; CurrentBoard = newBoardState; TurnNumber = game.GameBoard.TurnNumber+1; IsInverted = game.GameBoard.IsInverted}
        let newGame = {GameBoard = updatedBoard; Players = game.Players; Functions = game.Functions}
        playGame(newGame)
    else
        game.Functions.PrintFunction ("The game is over.  The computer is still unbeaten.")
        0


let startNewGame (input, print) =
    let game = askForGameInfo(print,input)
    let player1 = askForPlayerInformation(print, input, 1, 3)
    let player2 = askForPlayerInformation(print, input, 2, 3)
    {GameBoard = game; Players = [player1;player2]; Functions = {PrintFunction = print; InputFunction = input}}