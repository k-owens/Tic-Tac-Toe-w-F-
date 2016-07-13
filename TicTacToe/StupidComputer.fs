module StupidComputer
open GameBoard
open Game

let randomGenerator = System.Random()

let rec stupidComputerMove (game, player1 : Player, player2 : Player) = 
    let randomMove = randomGenerator.Next(0, game.BoardSize*game.BoardSize-1)
    if isLegalMove(randomMove,game) then
        randomMove
    else
        stupidComputerMove(game, player1, player2)