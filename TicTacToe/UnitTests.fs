module UnitTests

open Xunit
open FsUnit
open GamePlayer

[<Fact>]
let canGameBeStarted () =
    printfn ""

[<Fact>]
let canGameBePlayed () =
    printfn ""
    //
    //let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    //playGame(testBoard,1,1,1)

[<Fact>]
let canMakeMoveSpot1 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',1)

    Assert.AreEqual(newBoard, ['O';' ';' ';' ';' ';' ';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot2 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',2)

    Assert.AreEqual(newBoard, [' ';'O';' ';' ';' ';' ';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot3 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',3)

    Assert.AreEqual(newBoard, [' ';' ';'O';' ';' ';' ';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot4 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',4)

    Assert.AreEqual(newBoard, [' ';' ';' ';'O';' ';' ';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot5 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',5)

    Assert.AreEqual(newBoard, [' ';' ';' ';' ';'O';' ';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot6 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',6)

    Assert.AreEqual(newBoard, [' ';' ';' ';' ';' ';'O';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot7 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',7)

    Assert.AreEqual(newBoard, [' ';' ';' ';' ';' ';' ';'O';' ';' '])

[<Fact>]
let canMakeMoveSpot8 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',8)

    Assert.AreEqual(newBoard, [' ';' ';' ';' ';' ';' ';' ';'O';' '])

[<Fact>]
let canMakeMoveSpot9 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',9)

    Assert.AreEqual(newBoard, [' ';' ';' ';' ';' ';' ';' ';' ';'O'])

[<Fact>]
let canMakeHumanMove () = 
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = humanMove(1,testBoard,'O',true)

    Assert.AreEqual(newBoard, ['O';' ';' ';' ';' ';' ';' ';' ';' '])

[<Fact>]
let canMakeComputerMove () =
    let testBoard : char list = ['O';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = computerMove(testBoard, 1, 1,'O','X')

    Assert.AreEqual(newBoard, ['O';' ';' ';' ';'X';' ';' ';' ';' '])

[<Fact>]
let canComputerChooseCorner () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']
    Assert.AreEqual(chooseCorner(testBoard), 3)

[<Fact>]
let canComputerChooseSide () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']
    Assert.AreEqual(2, chooseSide(testBoard))

[<Fact>]
let canComputerChooseCornerInBetweenHumanMove () =
    let testBoard : char list = [' ';'O';' ';'O';' ';' ';' ';' ';' ']
    Assert.AreEqual(1, chooseCornerInBetween(4,testBoard,2))

[<Fact>]
let doesComputerKnowWhenFirstTurn () =
    let testBoard : char list = [' ';' ';' ';'O';' ';' ';' ';' ';' ']
    Assert.True(isFirstComputerTurn(testBoard,'O'))

[<Fact>]
let doesComputerKnowWhenNotFirstTurn () =
    let testBoard : char list = [' ';'O';' ';'O';' ';'X';' ';' ';' ']
    Assert.True(not(isFirstComputerTurn(testBoard,'O')))

[<Fact>]
let doesComputerRespondToFirstMoveMiddleCorrectly () = 
    let testBoard : char list = [' ';' ';' ';' ';'O';' ';' ';' ';' ']
    Assert.AreEqual(1, respondToFirstMoveMiddle(testBoard,5,'O'))

[<Fact>]
let doesComputerRespondToFirstMoveCornerCorrectly () = 
    let testBoard : char list = ['O';' ';' ';' ';' ';' ';' ';' ';' ']
    Assert.AreEqual(5, respondToFirstMoveCorner(testBoard,1,'O'))

[<Fact>]
let doesComputerRespondToFirstMoveSideCorrectly () = 
    let testBoard : char list = [' ';'O';' ';' ';' ';' ';' ';' ';' ']
    Assert.AreEqual(5, respondToFirstMoveSide(testBoard,'O'))

[<Fact>]
let doesComputerRespondToCorrectFirstMove () =
    let testBoard : char list = ['O';' ';' ';' ';' ';' ';' ';' ';' ']
    Assert.AreEqual(5, respondToFirstMove(testBoard,1,'O'))

[<Fact>]
let doesComputerRespondCorrectlyToMiddleStrategy () =
    let testBoard : char list = ['X';' ';' ';' ';'O';' ';' ';' ';'O']

    Assert.AreEqual(3,respondToMiddleStrategy(testBoard,5,'O','X'))

[<Fact>]
let doesComputerRespondCorrectlyToSideStrategy () =
    let testBoard : char list = [' ';'O';' ';' ';'X';'O';' ';' ';' ']

    Assert.AreEqual(3,respondToSideOrCornerStrategy(testBoard,5,2,'O','X'))

[<Fact>]
let doesComputerRespondCorrectlyToCornerStrategy () =
    let testBoard : char list = ['O';' ';' ';' ';'X';' ';' ';'O';' ']

    Assert.AreEqual(7,respondToSideOrCornerStrategy(testBoard,8,1,'O','X'))
    
[<Fact>]
let canNotOverrideMove () = 
    let testBoard : char list = ['O';' ';' ';' ';' ';' ';' ';' ';' ']
    let newBoard = makeMove(testBoard,'X',1)

    Assert.AreEqual(newBoard, ['O';' ';' ';' ';' ';' ';' ';' ';' '])

[<Fact>]
let canGameEnd () =
    let testBoard : char list = ['O';'X';'O';'O';'X';'O';'X';'O';'X']

    Assert.True(isGameOver(testBoard,'O','X'))

[<Fact>]
let canProgramEnd () =
    printfn ""

[<Fact>]
let canXWin () =
    let testBoard : char list = ['X';'X';'X';' ';' ';' ';' ';' ';' ']

    Assert.True(didComputerWin(testBoard,'X'))

[<Fact>]
let canOWin () =
    let testBoard : char list = ['O';'O';'O';' ';' ';' ';' ';' ';' ']

    Assert.True(didHumanWin(testBoard,'O'))

[<Fact>]
let canTie () = 
    let testBoard : char list = ['O';'X';'O';'O';'X';'O';'X';'O';'X']

    Assert.True(didTieHappen(testBoard))

[<Fact>]
let canWinHorizontal () =
    let testBoard : char list = ['X';'X';'X';' ';' ';' ';' ';' ';' ']

    Assert.True(didComputerWin(testBoard,'X'))

[<Fact>]
let canWinVertical () =
    let testBoard : char list = ['X';' ';' ';'X';' ';' ';'X';' ';' ']

    Assert.True(didComputerWin(testBoard,'X'))

[<Fact>]
let canWinDiagonal () =
    let testBoard : char list = ['X';' ';' ';' ';'X';' ';' ';' ';'X']

    Assert.True(didComputerWin(testBoard,'X'))

[<Fact>]
let testDisplay () =
    let testBoard : char list = ['X';' ';'O';' ';'O';' ';' ';' ';'X']

    displayBoardState(testBoard)

[<Fact>]
let canAskForInput () =
    askForInput (true)

[<Fact>]
let aiCanFindWin () =
    let testBoard : char list = ['X';' ';' ';'X';' ';' ';' ';' ';' ']

    Assert.AreEqual(7,winGameOrBlockWin('X',testBoard))

[<Fact>]
let aiCanFindHorizontalWins () =
    let testBoard : char list = ['X';'X';' ';' ';' ';' ';' ';' ';' ']

    Assert.AreEqual(2,checkHorizontalWins('X',testBoard))

[<Fact>]
let aiCanFindVerticalWins () =
    let testBoard : char list = ['X';' ';' ';'X';' ';' ';' ';' ';' ']

    Assert.AreEqual(6,checkVerticalWins('X',testBoard))

[<Fact>]
let aiCanFindDiagonalWins () =
    let testBoard : char list = ['X';' ';' ';' ';'X';' ';' ';' ';' ']

    Assert.AreEqual(8,checkDiagonalWins('X',testBoard))

[<Fact>]
let aiCanBlockHumanWin () =
    let testBoard : char list = ['O';' ';' ';' ';'O';' ';' ';' ';' ']

    Assert.AreEqual(8,checkDiagonalWins('O',testBoard))