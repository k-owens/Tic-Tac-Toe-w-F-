module MinimaxAlgorithm
open GameBoard

let score gameState player1 player2 boardSize=
    if didPlayer1Win gameState player1 boardSize then
        10
    elif didPlayer2Win gameState player2 boardSize then
        -10
    else
        0


let minimaxAlgorithm gameState=
    0