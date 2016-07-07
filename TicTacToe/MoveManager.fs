module MoveManager
open GameBoard
open RulesAlgorithm
open MinimaxAlgorithm

let rec humanMove (f,game, z : unit -> string, print : string -> unit) =
    let x = f(z,print)
    if isLegalMove (x, game) then
        x
    else
        humanMove (f,game,z,print)

let computerMove (gameState, isMinimax) =
    if isMinimax then
        minimaxAlgorithm gameState
    else
        rulesAlgorithm gameState