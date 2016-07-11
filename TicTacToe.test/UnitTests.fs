module UnitTests

open Xunit
open Game
open GameBoard 
open MoveManager 
open ConsoleTasks  
open GamePlayer
open UserSelection 
open MinimaxAlgorithm
open RuleAlgorithm3X3
open RuleAlgorithm4X4
open StupidComputer

let printHolder (arbitrary : string) =
    printfn ""

[<Fact>]
let canTie () =
    let game = {BoardSize = 3; CurrentBoard = [Some('O');Some('X');Some('O');Some('O');Some('X');Some('O');Some('X');Some('O');Some('X')]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    Assert.True(isGameOver (game))

[<Fact>]
let willNotEndGameTooSoon () =
    let game = {BoardSize = 3; CurrentBoard = [Some('O');None;Some('O');Some('O');Some('X');Some('O');Some('X');None;Some('X')]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    Assert.True(not(isGameOver (game)))

[<Fact>]
let willHorizontalWinEndGame () =
    let game1 = {BoardSize = 3; CurrentBoard = [Some('O');Some('O');Some('O');None;None;None;None;None;None]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    let game2 = {BoardSize = 3; CurrentBoard = [None;None;None;Some('O');Some('O');Some('O');None;None;None]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    let game3 = {BoardSize = 3; CurrentBoard = [None;None;None;None;None;None;Some('O');Some('O');Some('O')]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}

    Assert.True(isGameOver (game1))
    Assert.True(isGameOver (game2))
    Assert.True(isGameOver (game3))


[<Fact>]
let willVerticalWinEndGame () =
    let game1 = {BoardSize = 3; CurrentBoard = [Some('O');None;None;Some('O');None;None;Some('O');None;None]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    let game2 = {BoardSize = 3; CurrentBoard = [None;Some('O');None;None;Some('O');None;None;Some('O');None]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    let game3 = {BoardSize = 3; CurrentBoard = [None;None;Some('O');None;None;Some('O');None;None;Some('O')]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}

    Assert.True(isGameOver (game1))
    Assert.True(isGameOver (game2))
    Assert.True(isGameOver (game3))


[<Fact>]
let willDiagonalWinEndGame () =
    let game1 = {BoardSize = 3; CurrentBoard = [Some('O');None;None;None;Some('O');None;None;None;Some('O')]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    let game2 = {BoardSize = 3; CurrentBoard = [None;None;Some('O');None;Some('O');None;Some('O');None;None]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}

    Assert.True(isGameOver (game1))
    Assert.True(isGameOver (game2))

[<Fact>]
let canMakeMove () =
    let game = {BoardSize = 3; CurrentBoard = [None;None;None;None;None;None;None;None;None]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    Assert.Equal<char option list>([Some('O');None;None;None;None;None;None;None;None], makeMove (game, 0, 'O'))

[<Fact>]
let canDetermineIllegalMoves () =
    let game = {BoardSize = 3; CurrentBoard = [None;None;None;None;None;None;None;None;None]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    Assert.True(isLegalMove (8, game))
    Assert.True(not(isLegalMove (10,game)))
    Assert.True(not(isLegalMove (-1,game)))
    let game2 = {BoardSize = 4; CurrentBoard = [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None;None]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}

    Assert.True(isLegalMove (14, game2))

let returnStringNum () =
    "0"

let returnNum (f : unit -> string) =
    let x = returnStringNum()
    x |> System.Int32.Parse

[<Fact>]
let canHumanMakeMove () =
    let game = {BoardSize = 3; CurrentBoard = [None;None;None;None;None;None;None;None]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    Assert.Equal<int>(0,humanMove (moveInput, game, returnStringNum,printHolder))

(*[<Fact>]
let canComputerMakeMove () =
    let testGame = [None;None;None;None;None;None;None;None;None]
    Assert.Equal<int>(0,computerMove (testGame, true))*)

[<Fact>]
let canUserInputMove () =
    Assert.Equal<int>(0,moveInput(returnStringNum,printHolder))

[<Fact>]
let canDisplayInvertedBoard() =
    let testGame = [Some('O');Some('X');Some('O');Some('O');Some('X');Some('O');Some('X');Some('O');Some('X')]
    Assert.Equal<string>("Current board:\n___________\n|O|X|O|\n___________\n|O|X|O|\n___________\n|X|O|X|\n___________\n",displayBoard(testGame,3,true))

[<Fact>]
let canDisplayUninvertedBoard () =
    let testGame = [Some('O');Some('X');Some('O');Some('O');Some('X');Some('O');Some('X');Some('O');Some('X')]
    Assert.Equal<string>("Current board:\n___________\n|X|O|X|\n___________\n|O|X|O|\n___________\n|O|X|O|\n___________\n",displayBoard(testGame,3,false))

[<Fact>]
let canDisplayInvertedOptions () =
    Assert.Equal<string>("Board input:\n___________\n|0|1|2|\n___________\n|3|4|5|\n___________\n|6|7|8|\n___________\n",displayBoardOptions(3,true))

[<Fact>]
let canDisplayUninvertedOptions () =
    Assert.Equal<string>("Board input:\n___________\n8|7|6|\n___________\n5|4|3|\n___________\n2|1|0|\n___________\n",displayBoardOptions(3,false))

let returnString () =
    "a"

[<Fact>]
let canGetHumanCharacter () =
    Assert.Equal<char>('a',getHumanCharacter(printHolder,returnString))

[<Fact>]
let canGetComputerCharacter () =
    Assert.Equal<char>('a',getComputerCharacter(printHolder,returnString))

[<Fact>]
let canVerifySameCharacters () =
    Assert.True(not(verifyCharacters('a','a')))

[<Fact>]
let canVerifyDifferentCharacters () =
    Assert.True(verifyCharacters('a','b'))

(*[<Fact>]
let canGetCharactersForBothCharacters () =
    Assert.Equal<char list>(['a';'a'],getCharacterInput(printHolder,returnStringToChar))*)

[<Fact>]
let canDetermineWhoGoesFirst () =
    Assert.Equal<bool>(true,doesComputerGoFirst(printHolder,returnString))

[<Fact>]
let canPlayAgain () =
    Assert.Equal<bool>(false,askIfGameOver(printHolder,returnString))

[<Fact>]
let canaskIfInverted () =
    Assert.Equal<bool>(false,askIfInverted(printHolder,returnString))

let returnSeedNumber () =
    "1"

[<Fact>]
let canChooseAlgorithm () =
    Assert.Equal<int>(1,askAlgorithmSeed(printHolder,returnSeedNumber,3))

[<Fact>]
let canGetSizeOfBoard () =
    Assert.Equal<int>(4,getSizeOfBoard(printHolder,returnString))

[<Fact>]
let minimaxTest () =
    let game = {BoardSize = 3; CurrentBoard = [None;None;Some('O');None;None;None;None;None;None]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    Assert.Equal<int>(4,minimaxAlgorithm(game,true).[0])

[<Fact>]
let canComputerChooseCorner () =
    let testBoard : char option list = [None;None;None;None;None;None;None;None]
    Assert.Equal<int>(chooseCorner(testBoard).Value, 3)

[<Fact>]
let canComputerChooseSide () =
    let testBoard : char option list = [None;None;None;None;None;None;None;None]
    Assert.Equal<int>(2, chooseSide(testBoard).Value)

[<Fact>]
let canComputerChooseCornerInBetweenHumanMove () =
    let testBoard : char option list = [None;Some('O');None;Some('O');None;None;None;None;None]
    Assert.Equal<int>(1, chooseCornerInBetween(4,testBoard,2).Value)

[<Fact>]
let doesComputerKnowWhenFirstTurn () =
    let testBoard : char option list = [None;None;None;Some('O');None;None;None;None;None]
    Assert.True(isFirstComputerTurn(testBoard,'O'))

[<Fact>]
let doesComputerKnowWhenNotFirstTurn () =
    let testBoard : char option list = [None;Some('O');None;Some('O');None;None;None;None;None]
    Assert.True(not(isFirstComputerTurn(testBoard,'O')))

[<Fact>]
let doesComputerRespondToFirstMoveMiddleCorrectly () = 
    let testBoard : char option list = [None;None;None;None;Some('O');None;None;None;None]
    Assert.Equal<int>(1, respondToFirstMoveMiddle(testBoard,5,'O').Value)

[<Fact>]
let doesComputerRespondToFirstMoveCornerCorrectly () = 
    let testBoard : char option list = [Some('O');None;None;None;None;None;None;None;None]
    Assert.Equal<int>(5, respondToFirstMoveCorner(testBoard,1,'O').Value)

[<Fact>]
let doesComputerRespondToFirstMoveSideCorrectly () = 
    let testBoard : char option list = [None;Some('O');None;None;None;None;None;None;None]
    Assert.Equal<int>(5, respondToFirstMoveSide(testBoard,'O').Value)

[<Fact>]
let doesComputerRespondToCorrectFirstMove () =
    let testBoard : char option list = [Some('O');None;None;None;None;None;None;None;None]
    Assert.Equal<int>(5, respondToFirstMove(testBoard,1,'O').Value)

[<Fact>]
let canComputerChooseCornerSquare4X4 () =
    let testBoard = [None; None; None; None; None; None; None; None; None; None; None; None; None; None; None; None]

    Assert.Equal<int>(1,chooseCorner4X4(testBoard).Value)

[<Fact>]
let canComputerChooseSideSquare4X4 () =
    let testBoard = [None; None; None; None; None; None; None; None; None; None; None; None; None; None; None; None]

    Assert.Equal<int>(2,chooseSide4X4(testBoard).Value)

[<Fact>]
let canComputerFindHorizontalWins4X4 () =
    let testBoard1 = [None;Some('O');Some('O');Some('O');None;None;None;None;None;None;None;None;None;None;None;None]
    let testBoard2 = [Some('O');None;Some('O');Some('O');None;None;None;None;None;None;None;None;None;None;None;None]
    let testBoard3 = [Some('O');Some('O');None;Some('O');None;None;None;None;None;None;None;None;None;None;None;None]
    let testBoard4 = [Some('O');Some('O');Some('O');None;None;None;None;None;None;None;None;None;None;None;None;None]
    let testBoard5 = [None;None;None;None;None;Some('O');Some('O');Some('O');None;None;None;None;None;None;None;None]
    let testBoard6 = [None;None;None;None;Some('O');None;Some('O');Some('O');None;None;None;None;None;None;None;None]
    let testBoard7 = [None;None;None;None;Some('O');Some('O');None;Some('O');None;None;None;None;None;None;None;None]
    let testBoard8 = [None;None;None;None;Some('O');Some('O');Some('O');None;None;None;None;None;None;None;None;None]
    let testBoard9 = [None;None;None;None;None;None;None;None;None;Some('O');Some('O');Some('O');None;None;None;None]
    let testBoard10 = [None;None;None;None;None;None;None;None;Some('O');None;Some('O');Some('O');None;None;None;None]
    let testBoard11 = [None;None;None;None;None;None;None;None;Some('O');Some('O');None;Some('O');None;None;None;None]
    let testBoard12 = [None;None;None;None;None;None;None;None;Some('O');Some('O');Some('O');None;None;None;None;None]
    let testBoard13 = [None;None;None;None;None;None;None;None;None;None;None;None;None;Some('O');Some('O');Some('O')]
    let testBoard14 = [None;None;None;None;None;None;None;None;None;None;None;None;Some('O');None;Some('O');Some('O')]
    let testBoard15 = [None;None;None;None;None;None;None;None;None;None;None;None;Some('O');Some('O');None;Some('O')]
    let testBoard16 = [None;None;None;None;None;None;None;None;None;None;None;None;Some('O');Some('O');Some('O');None]
    let testBoard17 = [None;None;None;None;None;None;None;None;None;None;None;None;Some('O');Some('O');None;None]

    Assert.Equal<int>(1,checkHorizontalWins4X4(testBoard1,'O').Value)
    Assert.Equal<int>(2,checkHorizontalWins4X4(testBoard2,'O').Value)
    Assert.Equal<int>(3,checkHorizontalWins4X4(testBoard3,'O').Value)
    Assert.Equal<int>(4,checkHorizontalWins4X4(testBoard4,'O').Value)
    Assert.Equal<int>(5,checkHorizontalWins4X4(testBoard5,'O').Value)
    Assert.Equal<int>(6,checkHorizontalWins4X4(testBoard6,'O').Value)
    Assert.Equal<int>(7,checkHorizontalWins4X4(testBoard7,'O').Value)
    Assert.Equal<int>(8,checkHorizontalWins4X4(testBoard8,'O').Value)
    Assert.Equal<int>(9,checkHorizontalWins4X4(testBoard9,'O').Value)
    Assert.Equal<int>(10,checkHorizontalWins4X4(testBoard10,'O').Value)
    Assert.Equal<int>(11,checkHorizontalWins4X4(testBoard11,'O').Value)
    Assert.Equal<int>(12,checkHorizontalWins4X4(testBoard12,'O').Value)
    Assert.Equal<int>(13,checkHorizontalWins4X4(testBoard13,'O').Value)
    Assert.Equal<int>(14,checkHorizontalWins4X4(testBoard14,'O').Value)
    Assert.Equal<int>(15,checkHorizontalWins4X4(testBoard15,'O').Value)
    Assert.Equal<int>(16,checkHorizontalWins4X4(testBoard16,'O').Value)
    Assert.Equal<int option>(None,checkHorizontalWins4X4(testBoard17,'O'))

[<Fact>]
let canComputerFindVerticalWins4X4 () =
    let testBoard1 = [None;None;None;None;Some('O');None;None;None;Some('O');None;None;None;Some('O');None;None;None]
    let testBoard2 = [None;None;None;None;None;Some('O');None;None;None;Some('O');None;None;None;Some('O');None;None]
    let testBoard3 = [None;None;None;None;None;None;Some('O');None;None;None;Some('O');None;None;None;Some('O');None]
    let testBoard4 = [None;None;None;None;None;None;None;Some('O');None;None;None;Some('O');None;None;None;Some('O')]
    let testBoard5 = [Some('O');None;None;None;None;None;None;None;Some('O');None;None;None;Some('O');None;None;None]
    let testBoard6 = [None;Some('O');None;None;None;None;None;None;None;Some('O');None;None;None;Some('O');None;None]
    let testBoard7 = [None;None;Some('O');None;None;None;None;None;None;None;Some('O');None;None;None;Some('O');None]
    let testBoard8 = [None;None;None;Some('O');None;None;None;None;None;None;None;Some('O');None;None;None;Some('O')]
    let testBoard9 = [Some('O');None;None;None;Some('O');None;None;None;None;None;None;None;Some('O');None;None;None]
    let testBoard10 = [None;Some('O');None;None;None;Some('O');None;None;None;None;None;None;None;Some('O');None;None]
    let testBoard11 = [None;None;Some('O');None;None;None;Some('O');None;None;None;None;None;None;None;Some('O');None]
    let testBoard12 = [None;None;None;Some('O');None;None;None;Some('O');None;None;None;None;None;None;None;Some('O')]
    let testBoard13 = [Some('O');None;None;None;Some('O');None;None;None;Some('O');None;None;None;None;None;None;None]
    let testBoard14 = [None;Some('O');None;None;None;Some('O');None;None;None;Some('O');None;None;None;None;None;None]
    let testBoard15 = [None;None;Some('O');None;None;None;Some('O');None;None;None;Some('O');None;None;None;None;None]
    let testBoard16 = [None;None;None;Some('O');None;None;None;Some('O');None;None;None;Some('O');None;None;None;None]
    let testBoard17 = [None;None;None;Some('O');None;None;None;Some('O');None;None;None;None;None;None;None;None]

    Assert.Equal<int>(1,checkVerticalWins4X4(testBoard1, 'O').Value)
    Assert.Equal<int>(2,checkVerticalWins4X4(testBoard2, 'O').Value)
    Assert.Equal<int>(3,checkVerticalWins4X4(testBoard3, 'O').Value)
    Assert.Equal<int>(4,checkVerticalWins4X4(testBoard4, 'O').Value)
    Assert.Equal<int>(5,checkVerticalWins4X4(testBoard5, 'O').Value)
    Assert.Equal<int>(6,checkVerticalWins4X4(testBoard6, 'O').Value)
    Assert.Equal<int>(7,checkVerticalWins4X4(testBoard7, 'O').Value)
    Assert.Equal<int>(8,checkVerticalWins4X4(testBoard8, 'O').Value)
    Assert.Equal<int>(9,checkVerticalWins4X4(testBoard9, 'O').Value)
    Assert.Equal<int>(10,checkVerticalWins4X4(testBoard10, 'O').Value)
    Assert.Equal<int>(11,checkVerticalWins4X4(testBoard11, 'O').Value)
    Assert.Equal<int>(12,checkVerticalWins4X4(testBoard12, 'O').Value)
    Assert.Equal<int>(13,checkVerticalWins4X4(testBoard13, 'O').Value)
    Assert.Equal<int>(14,checkVerticalWins4X4(testBoard14, 'O').Value)
    Assert.Equal<int>(15,checkVerticalWins4X4(testBoard15, 'O').Value)
    Assert.Equal<int>(16,checkVerticalWins4X4(testBoard16, 'O').Value)
    Assert.Equal<int option>(None,checkVerticalWins4X4(testBoard17, 'O'))

[<Fact>]
let canComputerFindDiagonalWins () =
    let testBoard1 = [None;None;None;None;None;Some('O');None;None;None;None;Some('O');None;None;None;None;Some('O')]
    let testBoard2 = [Some('O');None;None;None;None;None;None;None;None;None;Some('O');None;None;None;None;Some('O')]
    let testBoard3 = [Some('O');None;None;None;None;Some('O');None;None;None;None;None;None;None;None;None;Some('O')]
    let testBoard4 = [Some('O');None;None;None;None;Some('O');None;None;None;None;Some('O');None;None;None;None;None]
    let testBoard5 = [None;None;None;None;None;None;Some('O');None;None;Some('O');None;None;Some('O');None;None;None]
    let testBoard6 = [None;None;None;Some('O');None;None;None;None;None;Some('O');None;None;Some('O');None;None;None]
    let testBoard7 = [None;None;None;Some('O');None;None;Some('O');None;None;None;None;None;Some('O');None;None;None]
    let testBoard8 = [None;None;None;Some('O');None;None;Some('O');None;None;Some('O');None;None;None;None;None;None]

    Assert.Equal<int>(1,checkDiagonalWins4X4(testBoard1,'O').Value)
    Assert.Equal<int>(6,checkDiagonalWins4X4(testBoard2,'O').Value)
    Assert.Equal<int>(11,checkDiagonalWins4X4(testBoard3,'O').Value)
    Assert.Equal<int>(16,checkDiagonalWins4X4(testBoard4,'O').Value)
    Assert.Equal<int>(4,checkDiagonalWins4X4(testBoard5,'O').Value)
    Assert.Equal<int>(7,checkDiagonalWins4X4(testBoard6,'O').Value)
    Assert.Equal<int>(10,checkDiagonalWins4X4(testBoard7,'O').Value)
    Assert.Equal<int>(13,checkDiagonalWins4X4(testBoard8,'O').Value)

[<Fact>]
let canStupidComputerMakeMove () =
    let game = {BoardSize = 3; CurrentBoard = [None;None;None;None;None;None;None;None;None]; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    let testMove = stupidComputerMove(game,game.BoardSize*game.BoardSize-1) 
    Assert.True(testMove > -1 && testMove < 9)