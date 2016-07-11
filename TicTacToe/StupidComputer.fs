module StupidComputer
open GameBoard
open Game

let randomGenerator = System.Random()

let rec stupidComputerMove (game, highestMove) = 
    let randomMove = randomGenerator.Next(0, highestMove)
    if isLegalMove(randomMove,game) then
        randomMove
    else
        stupidComputerMove(game, highestMove)