module UnitTests

open Xunit
open GamePlayer3X3
open GamePlayer4X4
open GameBoard

[<Fact>]
let canTie () =
    let testGame = ['O';'X';'O';'O';'X';'O';'X';'O';'X']
    Assert.True(isGameOver testGame 'O' 'X' 3)

[<Fact>]
let willNotEndGameTooSoon () =
    let testGame = ['O';' ';'O';'O';'X';'O';'X';' ';'X']

    Assert.True(not(isGameOver testGame 'O' 'X' 3))

[<Fact>]
let willHorizontalWinEndGame () =
    let testGame1 = ['O';'O';'O';' ';' ';' ';' ';' ';' ']
    let testGame2 = [' ';' ';' ';'O';'O';'O';' ';' ';' ']
    let testGame3 = [' ';' ';' ';' ';' ';' ';'O';'O';'O']


    Assert.True(isGameOver testGame1 'O' 'X' 3)
    Assert.True(isGameOver testGame2 'O' 'X' 3)
    Assert.True(isGameOver testGame3 'O' 'X' 3)


[<Fact>]
let willVerticalWinEndGame () =
    let testGame1 = ['O';' ';' ';'O';' ';' ';'O';' ';' ']
    let testGame2 = [' ';'O';' ';' ';'O';' ';' ';'O';' ']
    let testGame3 = [' ';' ';'O';' ';' ';'O';' ';' ';'O']


    Assert.True(isGameOver testGame1 'O' 'X' 3)
    Assert.True(isGameOver testGame2 'O' 'X' 3)
    Assert.True(isGameOver testGame3 'O' 'X' 3)


[<Fact>]
let willDiagonalWinEndGame () =
    let testGame1 = ['O';' ';' ';' ';'O';' ';' ';' ';'O']
    let testGame2 = [' ';' ';'O';' ';'O';' ';'O';' ';' ']


    Assert.True(isGameOver testGame1 'O' 'X' 3)
    Assert.True(isGameOver testGame2 'O' 'X' 3)

[<Fact>]
let canMakeMove () =
    let testGame = [' ';' ';' ';' ';' ';' ';' ';' ';' ']
    Assert.Equal<char list>(['O';' ';' ';' ';' ';' ';' ';' ';' '], makeMove (testGame, 3, 0, 'O'))

[<Fact>]
let canDetermineIllegalMoves () =
    Assert.True(isLegalMove 8 3)
    Assert.True(not(isLegalMove 10 3))
    Assert.True(not(isLegalMove -1 3))
    Assert.True(isLegalMove 14 4)