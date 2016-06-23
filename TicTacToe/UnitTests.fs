module UnitTests

open NUnit.Framework
open FsUnit
open GamePlayer

[<Test>]
let canMakeMove () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',1)

    Assert.AreEqual(newBoard, ['O';' ';' ';' ';' ';' ';' ';' ';' '])

[<Test>]
let canMakeHumanMove () = 
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = humanMove(1,testBoard)

    Assert.AreEqual(newBoard, ['O';' ';' ';' ';' ';' ';' ';' ';' '])

[<Test>]
let canMakeComputerMove () =
    let testBoard : char list = ['O';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = computerMove(testBoard, 1, 1)

    Assert.AreEqual(newBoard, ['O';' ';' ';' ';'X';' ';' ';' ';' '])

[<Test>]
let canNotOverrideMove () = 
    let testBoard : char list = ['O';' ';' ';' ';' ';' ';' ';' ';' ']
    let newBoard = makeMove(testBoard,'X',1)

    Assert.AreEqual(newBoard, ['O';' ';' ';' ';' ';' ';' ';' ';' '])

[<Test>]
let canXWin () =
    let testBoard : char list = ['X';'X';'X';' ';' ';' ';' ';' ';' ']

    Assert.IsTrue(didXWin(testBoard))

[<Test>]
let canOWin () =
    let testBoard : char list = ['O';'O';'O';' ';' ';' ';' ';' ';' ']

    Assert.IsTrue(didOWin(testBoard))

[<Test>]
let canTie () = 
    let testBoard : char list = ['O';'X';'O';'O';'X';'O';'X';'O';'X']

    Assert.IsTrue(didTieHappen(testBoard))

[<Test>]
let canWinHorizontal () =
    let testBoard : char list = ['X';'X';'X';' ';' ';' ';' ';' ';' ']

    Assert.IsTrue(didXWin(testBoard))

[<Test>]
let canWinVertical () =
    let testBoard : char list = ['X';' ';' ';'X';' ';' ';'X';' ';' ']

    Assert.IsTrue(didXWin(testBoard))

[<Test>]
let canWinDiagonal () =
    let testBoard : char list = ['X';' ';' ';' ';'X';' ';' ';' ';'X']

    Assert.IsTrue(didXWin(testBoard))