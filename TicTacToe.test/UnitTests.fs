//namespace TicTacToe.test
module UnitTests

open Xunit
open GamePlayer3X3
open GamePlayer4X4

[<Fact>]
let canGameBeStarted () =
    printfn ""

[<Fact>]
let canGameBePlayed () =
    printfn ""

[<Fact>]
let canMakeMoveSpot1 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',1)

    Assert.Equal<char list>(newBoard, ['O';' ';' ';' ';' ';' ';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot2 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',2)

    Assert.Equal<char list>(newBoard, [' ';'O';' ';' ';' ';' ';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot3 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',3)

    Assert.Equal<char list>(newBoard, [' ';' ';'O';' ';' ';' ';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot4 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',4)

    Assert.Equal<char list>(newBoard, [' ';' ';' ';'O';' ';' ';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot5 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',5)

    Assert.Equal<char list>(newBoard, [' ';' ';' ';' ';'O';' ';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot6 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',6)

    Assert.Equal<char list>(newBoard, [' ';' ';' ';' ';' ';'O';' ';' ';' '])

[<Fact>]
let canMakeMoveSpot7 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',7)

    Assert.Equal<char list>(newBoard, [' ';' ';' ';' ';' ';' ';'O';' ';' '])

[<Fact>]
let canMakeMoveSpot8 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',8)

    Assert.Equal<char list>(newBoard, [' ';' ';' ';' ';' ';' ';' ';'O';' '])

[<Fact>]
let canMakeMoveSpot9 () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = makeMove(testBoard,'O',9)

    Assert.Equal<char list>(newBoard, [' ';' ';' ';' ';' ';' ';' ';' ';'O'])

[<Fact>]
let canMakeHumanMove () = 
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = humanMove(1,testBoard,'O',true)
    let newBoard2 = humanMove(9,testBoard,'O',true)

    Assert.Equal<char list>(newBoard, ['O';' ';' ';' ';' ';' ';' ';' ';' '])
    Assert.Equal<char list>(newBoard2, [' ';' ';' ';' ';' ';' ';' ';' ';'O'])


[<Fact>]
let canMakeComputerMove () =
    let testBoard : char list = ['O';' ';' ';' ';' ';' ';' ';' ';' ']

    let newBoard = computerMove(testBoard, 1, 1,'O','X')

    Assert.Equal<char list>(newBoard, ['O';' ';' ';' ';'X';' ';' ';' ';' '])

[<Fact>]
let canComputerChooseCorner () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']
    Assert.Equal<int>(chooseCorner(testBoard), 3)

[<Fact>]
let canComputerChooseSide () =
    let testBoard : char list = [' ';' ';' ';' ';' ';' ';' ';' ';' ']
    Assert.Equal<int>(2, chooseSide(testBoard))

[<Fact>]
let canComputerChooseCornerInBetweenHumanMove () =
    let testBoard : char list = [' ';'O';' ';'O';' ';' ';' ';' ';' ']
    Assert.Equal<int>(1, chooseCornerInBetween(4,testBoard,2))

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
    Assert.Equal<int>(1, respondToFirstMoveMiddle(testBoard,5,'O'))

[<Fact>]
let doesComputerRespondToFirstMoveCornerCorrectly () = 
    let testBoard : char list = ['O';' ';' ';' ';' ';' ';' ';' ';' ']
    Assert.Equal<int>(5, respondToFirstMoveCorner(testBoard,1,'O'))

[<Fact>]
let doesComputerRespondToFirstMoveSideCorrectly () = 
    let testBoard : char list = [' ';'O';' ';' ';' ';' ';' ';' ';' ']
    Assert.Equal<int>(5, respondToFirstMoveSide(testBoard,'O'))

[<Fact>]
let doesComputerRespondToCorrectFirstMove () =
    let testBoard : char list = ['O';' ';' ';' ';' ';' ';' ';' ';' ']
    Assert.Equal<int>(5, respondToFirstMove(testBoard,1,'O'))

[<Fact>]
let doesComputerRespondCorrectlyToMiddleStrategy () =
    let testBoard : char list = ['X';' ';' ';' ';'O';' ';' ';' ';'O']

    Assert.Equal<int>(3,respondToMiddleStrategy(testBoard,5,'O','X'))

[<Fact>]
let doesComputerRespondCorrectlyToSideStrategy () =
    let testBoard : char list = [' ';'O';' ';' ';'X';'O';' ';' ';' ']

    Assert.Equal<int>(3,respondToSideOrCornerStrategy(testBoard,5,2,'O','X'))

[<Fact>]
let doesComputerRespondCorrectlyToCornerStrategy () =
    let testBoard : char list = ['O';' ';' ';' ';'X';' ';' ';'O';' ']

    Assert.Equal<int>(7,respondToSideOrCornerStrategy(testBoard,8,1,'O','X'))
    
[<Fact>]
let canNotOverrideMove () = 
    let testBoard : char list = ['O';' ';' ';' ';' ';' ';' ';' ';' ']
    let newBoard = makeMove(testBoard,'X',1)

    Assert.Equal<char list>(newBoard, ['O';' ';' ';' ';' ';' ';' ';' ';' '])

[<Fact>]
let canGameEnd () =
    let testBoard : char list = ['O';'X';'O';'O';'X';'O';'X';'O';'X']

    Assert.True(isGameOver(testBoard,'O','X'))

[<Fact>]
let canProgramEnd () =
    printfn ""

[<Fact>]
let canPlayer1Win () =
    let testBoard : char list = ['X';'X';'X';' ';' ';' ';' ';' ';' ']

    Assert.True(didSomeoneWin(testBoard,'X'))

[<Fact>]
let canPlayer2Win () =
    let testBoard : char list = ['O';'O';'O';' ';' ';' ';' ';' ';' ']

    Assert.True(didSomeoneWin(testBoard,'O'))

[<Fact>]
let canTie () = 
    let testBoard : char list = ['O';'X';'O';'O';'X';'O';'X';'O';'X']

    Assert.True(didTieHappen(testBoard))

[<Fact>]
let willTieHappenBeforeBoardIsFull () =
    let testBoard : char list = [' ';'X';'O';'O';'X';'O';'X';'O';'X']

    Assert.True(not(didTieHappen(testBoard)))

[<Fact>]
let canWinHorizontal () =
    let testBoard : char list = ['X';'X';'X';' ';' ';' ';' ';' ';' ']
    let testBoard2 : char list = [' ';' ';' ';'X';'X';'X';' ';' ';' ']
    let testBoard3 : char list = [' ';' ';' ';' ';' ';' ';'X';'X';'X']

    Assert.True(didSomeoneWin(testBoard,'X'))
    Assert.True(didSomeoneWin(testBoard2,'X'))
    Assert.True(didSomeoneWin(testBoard3,'X'))


[<Fact>]
let canWinVertical () =
    let testBoard : char list = ['X';' ';' ';'X';' ';' ';'X';' ';' ']
    let testBoard2 : char list = [' ';'X';' ';' ';'X';' ';' ';'X';' ']
    let testBoard3 : char list = [' ';' ';'X';' ';' ';'X';' ';' ';'X']


    Assert.True(didSomeoneWin(testBoard,'X'))
    Assert.True(didSomeoneWin(testBoard2,'X'))
    Assert.True(didSomeoneWin(testBoard3,'X'))

[<Fact>]
let canWinDiagonal () =
    let testBoard : char list = ['X';' ';' ';' ';'X';' ';' ';' ';'X']
    let testBoard2 : char list = [' ';' ';'X';' ';'X';' ';'X';' ';' ']


    Assert.True(didSomeoneWin(testBoard,'X'))
    Assert.True(didSomeoneWin(testBoard2,'X'))

[<Fact>]
let testDisplay () =
    let testBoard : char list = ['X';' ';'O';' ';'O';' ';' ';' ';'X']

    displayBoardState(testBoard)

[<Fact>]
let canAskForInput () =
    let y = askForInput (true)
    Assert.Equal<int>(0,y)

[<Fact>]
let canAskForInputOnInvertedBoard () = 
    let y = askForInput (false)
    Assert.Equal<int>(0,y)

[<Fact>]
let aiCanFindWin () =
    let testBoard : char list = ['X';' ';' ';'X';' ';' ';' ';' ';' ']

    Assert.Equal<int>(7,winGameOrBlockWin('X',testBoard))

[<Fact>]
let aiCanFindHorizontalWins () =
    let testBoard : char list = ['X';'X';' ';' ';' ';' ';' ';' ';' ']

    Assert.Equal<int>(2,checkHorizontalWins('X',testBoard))

[<Fact>]
let aiCanFindVerticalWins () =
    let testBoard : char list = ['X';' ';' ';'X';' ';' ';' ';' ';' ']

    Assert.Equal<int>(6,checkVerticalWins('X',testBoard))

[<Fact>]
let aiCanFindDiagonalWins () =
    let testBoard : char list = ['X';' ';' ';' ';'X';' ';' ';' ';' ']

    Assert.Equal<int>(8,checkDiagonalWins('X',testBoard))

[<Fact>]
let aiCanBlockHumanWin () =
    let testBoard : char list = ['O';' ';' ';' ';'O';' ';' ';' ';' ']

    Assert.Equal<int>(8,checkDiagonalWins('O',testBoard))


(*4X4 Tests*)

[<Fact>]
let canMakeMoveOn4X4 () = 
    let testBoard = [' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']

    Assert.Equal<char list>(['O'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',1))
    Assert.Equal<char list>([' '; 'O'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',2))
    Assert.Equal<char list>([' '; ' '; 'O'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',3))
    Assert.Equal<char list>([' '; ' '; ' '; 'O'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',4))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; 'O'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',5))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; 'O'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',6))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; ' '; 'O'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',7))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; ' '; ' '; 'O'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',8))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'O'; ' '; ' '; ' '; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',9))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'O'; ' '; ' '; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',10))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'O'; ' '; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',11))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'O'; ' '; ' '; ' '; ' '],makeMove4X4(testBoard,'O',12))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'O'; ' '; ' '; ' '],makeMove4X4(testBoard,'O',13))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'O'; ' '; ' '],makeMove4X4(testBoard,'O',14))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'O'; ' '],makeMove4X4(testBoard,'O',15))
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; 'O'],makeMove4X4(testBoard,'O',16))

[<Fact>]
let testDisplay4X4 () =
    let testBoard : char list = ['X';' ';'O';' ';'O';' ';' ';' ';'X';'X';' ';'O';' ';'O';' ';' ';' ';]

    displayBoardState4X4(testBoard)

[<Fact>]
let canAskForInput4X4 () =
    askForInput4X4 (true)    

[<Fact>]
let canAskForInputOnInvertedBoard4X4 () =
    askForInput4X4 (false)    

[<Fact>]
let canGameEnd4X4 () =
    let testBoard : char list = ['O';'X';'O';'O';'X';'O';'X';'O';'X';'O';'X';'O';'O';'X';'O';'X';'O';]

    Assert.True(isGameOver4X4(testBoard,'O','X'))

[<Fact>]
let canTie4X4 () = 
    let testBoard : char list = ['O';'X';'O';'O';'X';'O';'X';'O';'X';'O';'X';'O';'O';'X';'O';'X';'O';]

    Assert.True(didTieHappen4X4(testBoard))

[<Fact>]
let willTieHappenBeforeBoardIsFull4X4 () =
    let testBoard : char list = [' ';'X';'O';'O';'X';'O';'X';'O';'X';' ';'X';'O';'O';'X';'O';'X';'O';]

    Assert.True(not(didTieHappen4X4(testBoard)))

[<Fact>]
let canSomeoneWin4X4 () = 
    let testBoard1 = ['O';'O';'O';'O';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' '] 
    let testBoard2 = [' ';' ';' ';' ';'O';'O';'O';'O';' ';' ';' ';' ';' ';' ';' ';' '] 
    let testBoard3 = [' ';' ';' ';' ';' ';' ';' ';' ';'O';'O';'O';'O';' ';' ';' ';' '] 
    let testBoard4 = [' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';'O';'O';'O';'O'] 
    let testBoard5 = ['O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' '] 
    let testBoard6 = [' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' '] 
    let testBoard7 = [' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' '] 
    let testBoard8 = [' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O'] 
    let testBoard9 = ['O';' ';' ';' ';' ';'O';' ';' ';' ';' ';'O';' ';' ';' ';' ';'O'] 
    let testBoard10 = [' ';' ';' ';'O';' ';' ';'O';' ';' ';'O';' ';' ';'O';' ';' ';' ']

    Assert.True(didSomeoneWin4X4(testBoard1, 'O'))
    Assert.True(didSomeoneWin4X4(testBoard2, 'O'))
    Assert.True(didSomeoneWin4X4(testBoard3, 'O'))
    Assert.True(didSomeoneWin4X4(testBoard4, 'O'))
    Assert.True(didSomeoneWin4X4(testBoard5, 'O'))
    Assert.True(didSomeoneWin4X4(testBoard6, 'O'))
    Assert.True(didSomeoneWin4X4(testBoard7, 'O'))
    Assert.True(didSomeoneWin4X4(testBoard8, 'O'))
    Assert.True(didSomeoneWin4X4(testBoard9, 'O'))
    Assert.True(didSomeoneWin4X4(testBoard10, 'O'))

[<Fact>]
let canHumanMakeMove4X4 () = 
    let testBoard = [' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']
    Assert.Equal<char list>(['O'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '],humanMove4X4(1,testBoard,'O',true))

[<Fact>]
let canComputerChooseMiddleSquare4X4 () =
    let testBoard = [' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']

    Assert.Equal<int>(6,chooseMiddle4X4(testBoard))

[<Fact>]
let canComputerChooseCornerSquare4X4 () =
    let testBoard = [' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']

    Assert.Equal<int>(1,chooseCorner4X4(testBoard))

[<Fact>]
let canComputerChooseSideSquare4X4 () =
    let testBoard = [' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']

    Assert.Equal<int>(2,chooseSide4X4(testBoard))

[<Fact>]
let canComputerFindHorizontalWins4X4 () =
    let testBoard1 = [' ';'O';'O';'O';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ']
    let testBoard2 = ['O';' ';'O';'O';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ']
    let testBoard3 = ['O';'O';' ';'O';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ']
    let testBoard4 = ['O';'O';'O';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ']
    let testBoard5 = [' ';' ';' ';' ';' ';'O';'O';'O';' ';' ';' ';' ';' ';' ';' ';' ']
    let testBoard6 = [' ';' ';' ';' ';'O';' ';'O';'O';' ';' ';' ';' ';' ';' ';' ';' ']
    let testBoard7 = [' ';' ';' ';' ';'O';'O';' ';'O';' ';' ';' ';' ';' ';' ';' ';' ']
    let testBoard8 = [' ';' ';' ';' ';'O';'O';'O';' ';' ';' ';' ';' ';' ';' ';' ';' ']
    let testBoard9 = [' ';' ';' ';' ';' ';' ';' ';' ';' ';'O';'O';'O';' ';' ';' ';' ']
    let testBoard10 = [' ';' ';' ';' ';' ';' ';' ';' ';'O';' ';'O';'O';' ';' ';' ';' ']
    let testBoard11 = [' ';' ';' ';' ';' ';' ';' ';' ';'O';'O';' ';'O';' ';' ';' ';' ']
    let testBoard12 = [' ';' ';' ';' ';' ';' ';' ';' ';'O';'O';'O';' ';' ';' ';' ';' ']
    let testBoard13 = [' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';'O';'O';'O']
    let testBoard14 = [' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';'O';' ';'O';'O']
    let testBoard15 = [' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';'O';'O';' ';'O']
    let testBoard16 = [' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';'O';'O';'O';' ']
    let testBoard17 = [' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';'O';'O';' ';' ']

    Assert.Equal<int>(1,checkHorizontalWins4X4(testBoard1,'O'))
    Assert.Equal<int>(2,checkHorizontalWins4X4(testBoard2,'O'))
    Assert.Equal<int>(3,checkHorizontalWins4X4(testBoard3,'O'))
    Assert.Equal<int>(4,checkHorizontalWins4X4(testBoard4,'O'))
    Assert.Equal<int>(5,checkHorizontalWins4X4(testBoard5,'O'))
    Assert.Equal<int>(6,checkHorizontalWins4X4(testBoard6,'O'))
    Assert.Equal<int>(7,checkHorizontalWins4X4(testBoard7,'O'))
    Assert.Equal<int>(8,checkHorizontalWins4X4(testBoard8,'O'))
    Assert.Equal<int>(9,checkHorizontalWins4X4(testBoard9,'O'))
    Assert.Equal<int>(10,checkHorizontalWins4X4(testBoard10,'O'))
    Assert.Equal<int>(11,checkHorizontalWins4X4(testBoard11,'O'))
    Assert.Equal<int>(12,checkHorizontalWins4X4(testBoard12,'O'))
    Assert.Equal<int>(13,checkHorizontalWins4X4(testBoard13,'O'))
    Assert.Equal<int>(14,checkHorizontalWins4X4(testBoard14,'O'))
    Assert.Equal<int>(15,checkHorizontalWins4X4(testBoard15,'O'))
    Assert.Equal<int>(16,checkHorizontalWins4X4(testBoard16,'O'))
    Assert.Equal<int>(-1,checkHorizontalWins4X4(testBoard17,'O'))

[<Fact>]
let canComputerFindVerticalWins4X4 () =
    let testBoard1 = [' ';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ']
    let testBoard2 = [' ';' ';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ']
    let testBoard3 = [' ';' ';' ';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' ']
    let testBoard4 = [' ';' ';' ';' ';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O']
    let testBoard5 = ['O';' ';' ';' ';' ';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ']
    let testBoard6 = [' ';'O';' ';' ';' ';' ';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ']
    let testBoard7 = [' ';' ';'O';' ';' ';' ';' ';' ';' ';' ';'O';' ';' ';' ';'O';' ']
    let testBoard8 = [' ';' ';' ';'O';' ';' ';' ';' ';' ';' ';' ';'O';' ';' ';' ';'O']
    let testBoard9 = ['O';' ';' ';' ';'O';' ';' ';' ';' ';' ';' ';' ';'O';' ';' ';' ']
    let testBoard10 = [' ';'O';' ';' ';' ';'O';' ';' ';' ';' ';' ';' ';' ';'O';' ';' ']
    let testBoard11 = [' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';' ';' ';' ';' ';'O';' ']
    let testBoard12 = [' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';' ';' ';' ';' ';'O']
    let testBoard13 = ['O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';' ';' ';' ';' ']
    let testBoard14 = [' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';' ';' ';' ']
    let testBoard15 = [' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';' ';' ']
    let testBoard16 = [' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';' ']
    let testBoard17 = [' ';' ';' ';'O';' ';' ';' ';'O';' ';' ';' ';' ';' ';' ';' ';' ']

    Assert.Equal<int>(1,checkVerticalWins4X4(testBoard1, 'O'))
    Assert.Equal<int>(2,checkVerticalWins4X4(testBoard2, 'O'))
    Assert.Equal<int>(3,checkVerticalWins4X4(testBoard3, 'O'))
    Assert.Equal<int>(4,checkVerticalWins4X4(testBoard4, 'O'))
    Assert.Equal<int>(5,checkVerticalWins4X4(testBoard5, 'O'))
    Assert.Equal<int>(6,checkVerticalWins4X4(testBoard6, 'O'))
    Assert.Equal<int>(7,checkVerticalWins4X4(testBoard7, 'O'))
    Assert.Equal<int>(8,checkVerticalWins4X4(testBoard8, 'O'))
    Assert.Equal<int>(9,checkVerticalWins4X4(testBoard9, 'O'))
    Assert.Equal<int>(10,checkVerticalWins4X4(testBoard10, 'O'))
    Assert.Equal<int>(11,checkVerticalWins4X4(testBoard11, 'O'))
    Assert.Equal<int>(12,checkVerticalWins4X4(testBoard12, 'O'))
    Assert.Equal<int>(13,checkVerticalWins4X4(testBoard13, 'O'))
    Assert.Equal<int>(14,checkVerticalWins4X4(testBoard14, 'O'))
    Assert.Equal<int>(15,checkVerticalWins4X4(testBoard15, 'O'))
    Assert.Equal<int>(16,checkVerticalWins4X4(testBoard16, 'O'))
    Assert.Equal<int>(-1,checkVerticalWins4X4(testBoard17, 'O'))

[<Fact>]
let canComputerFindDiagonalWins () =
    let testBoard1 = [' ';' ';' ';' ';' ';'O';' ';' ';' ';' ';'O';' ';' ';' ';' ';'O']
    let testBoard2 = ['O';' ';' ';' ';' ';' ';' ';' ';' ';' ';'O';' ';' ';' ';' ';'O']
    let testBoard3 = ['O';' ';' ';' ';' ';'O';' ';' ';' ';' ';' ';' ';' ';' ';' ';'O']
    let testBoard4 = ['O';' ';' ';' ';' ';'O';' ';' ';' ';' ';'O';' ';' ';' ';' ';' ']
    let testBoard5 = [' ';' ';' ';' ';' ';' ';'O';' ';' ';'O';' ';' ';'O';' ';' ';' ']
    let testBoard6 = [' ';' ';' ';'O';' ';' ';' ';' ';' ';'O';' ';' ';'O';' ';' ';' ']
    let testBoard7 = [' ';' ';' ';'O';' ';' ';'O';' ';' ';' ';' ';' ';'O';' ';' ';' ']
    let testBoard8 = [' ';' ';' ';'O';' ';' ';'O';' ';' ';'O';' ';' ';' ';' ';' ';' ']
    let testBoard9 = [' ';' ';' ';' ';' ';' ';'O';' ';' ';'O';' ';' ';' ';' ';' ';' ']

    Assert.Equal<int>(1,checkDiagonalWins4X4(testBoard1,'O'))
    Assert.Equal<int>(6,checkDiagonalWins4X4(testBoard2,'O'))
    Assert.Equal<int>(11,checkDiagonalWins4X4(testBoard3,'O'))
    Assert.Equal<int>(16,checkDiagonalWins4X4(testBoard4,'O'))
    Assert.Equal<int>(4,checkDiagonalWins4X4(testBoard5,'O'))
    Assert.Equal<int>(7,checkDiagonalWins4X4(testBoard6,'O'))
    Assert.Equal<int>(10,checkDiagonalWins4X4(testBoard7,'O'))
    Assert.Equal<int>(13,checkDiagonalWins4X4(testBoard8,'O'))
    Assert.Equal<int>(-1,checkDiagonalWins4X4(testBoard9,'O'))

[<Fact>]
let canComputerMakeMove () =
    let testBoard = [' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']
    Assert.Equal<char list>([' '; ' '; ' '; ' '; ' '; 'O'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '],computerMove4X4(testBoard,'X','O'))
