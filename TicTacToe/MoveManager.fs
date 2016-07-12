module MoveManager
open Game
open GameBoard
open MinimaxAlgorithm
open RuleAlgorithm3X3
open RuleAlgorithm4X4
open StupidComputer


let rec humanMove (input,game, enterMove : unit -> string, print : string -> unit) =
    let move = input(enterMove,print)
    if isLegalMove (move, game) then
        move
    else
        humanMove (input,game,enterMove,print)

let computerMove (gameState : Game, algorithmSeed,  print : string -> unit, askingPlayer, opposingPlayer) : int =
    print ("Computer Move:\n")
    if algorithmSeed = 1  then
        let result = minimaxAlgorithm (gameState,true, askingPlayer, opposingPlayer)
        result.[0]
    elif(algorithmSeed = 3 && gameState.BoardSize = 3) then
        rule3X3(gameState,0,0,opposingPlayer.PlayerCharacter,askingPlayer.PlayerCharacter)
    elif(algorithmSeed = 3 && gameState.BoardSize = 4) then
        rule4X4(gameState,opposingPlayer.PlayerCharacter,askingPlayer.PlayerCharacter)
    else
        stupidComputerMove(gameState, gameState.BoardSize*gameState.BoardSize-1)
        
let moveTypeSelector (player : Player,input,game, enterMove : unit -> string, print : string -> unit,gameState : Game, algorithmSeed, opposingPlayer) =
    if(player.PlayerSeed = 1) then
        humanMove (input,game, enterMove, print)
    else
        computerMove (gameState, player.ComputerAlgorithm, print, player,opposingPlayer)