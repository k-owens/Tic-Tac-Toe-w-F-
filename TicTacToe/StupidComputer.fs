module StupidComputer
open GameBoard
open Game

let y = System.Random()

let rec stupidComputerMove (game, highestMove) = 
    let x = y.Next(0, highestMove)
    if isLegalMove(x,game) then
        x
    else
        stupidComputerMove(game, highestMove)