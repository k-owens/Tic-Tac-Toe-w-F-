module MoveManager
open Game
open GameBoard
open MinimaxAlgorithm
open RuleAlgorithm3X3
open RuleAlgorithm4X4

let rec humanMove (input,game, enterMove : unit -> string, print : string -> unit) =
    let move = input(enterMove,print)
    if isLegalMove (move, game) then
        move
    else
        humanMove (input,game,enterMove,print)

let computerMove (gameState : Game, isMinimax) : int =
    if isMinimax || not(gameState.BoardSize = 3 && gameState.BoardSize = 4)  then
        let result = minimaxAlgorithm (gameState,true)
        result.[0]
    elif(not(gameState.BoardSize = 3)) then
        rule3X3(gameState,0,0,gameState.HumanCharacter,gameState.ComputerCharacter)
    else
        rule4X4(gameState, gameState.HumanCharacter, gameState.ComputerCharacter)