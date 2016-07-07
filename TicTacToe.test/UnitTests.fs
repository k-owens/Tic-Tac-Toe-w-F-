module UnitTests

open Xunit
open GamePlayer3X3
open GamePlayer4X4
open Game
open GameBoard // done
open MoveManager // done
open ConsoleTasks  //done
open GamePlayer // in progess (should work once the algorithm is complete)
open UserSelection // in progress

let printHolder (arbitrary : string) =
    printfn ""

[<Fact>]
let canTie () =
    let game = {BoardSize = 3; CurrentBoard = ['O';'X';'O';'O';'X';'O';'X';'O';'X']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    Assert.True(isGameOver (game))

[<Fact>]
let willNotEndGameTooSoon () =
    let game = {BoardSize = 3; CurrentBoard = ['O';' ';'O';'O';'X';'O';'X';' ';'X']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    Assert.True(not(isGameOver (game)))

[<Fact>]
let willHorizontalWinEndGame () =
    let game1 = {BoardSize = 3; CurrentBoard = ['O';'O';'O';' ';' ';' ';' ';' ';' ']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    let game2 = {BoardSize = 3; CurrentBoard = [' ';' ';' ';'O';'O';'O';' ';' ';' ']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    let game3 = {BoardSize = 3; CurrentBoard = [' ';' ';' ';' ';' ';' ';'O';'O';'O']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}

    Assert.True(isGameOver (game1))
    Assert.True(isGameOver (game2))
    Assert.True(isGameOver (game3))


[<Fact>]
let willVerticalWinEndGame () =
    let game1 = {BoardSize = 3; CurrentBoard = ['O';' ';' ';'O';' ';' ';'O';' ';' ']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    let game2 = {BoardSize = 3; CurrentBoard = [' ';'O';' ';' ';'O';' ';' ';'O';' ']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    let game3 = {BoardSize = 3; CurrentBoard = [' ';' ';'O';' ';' ';'O';' ';' ';'O']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}

    Assert.True(isGameOver (game1))
    Assert.True(isGameOver (game2))
    Assert.True(isGameOver (game3))


[<Fact>]
let willDiagonalWinEndGame () =
    let game1 = {BoardSize = 3; CurrentBoard = ['O';' ';' ';' ';'O';' ';' ';' ';'O']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    let game2 = {BoardSize = 3; CurrentBoard = [' ';' ';'O';' ';'O';' ';'O';' ';' ']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}

    Assert.True(isGameOver (game1))
    Assert.True(isGameOver (game2))

[<Fact>]
let canMakeMove () =
    let game = {BoardSize = 3; CurrentBoard = [' ';' ';' ';' ';' ';' ';' ';' ';' ']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    Assert.Equal<char list>(['O';' ';' ';' ';' ';' ';' ';' ';' '], makeMove (game, 0, 'O'))

[<Fact>]
let canDetermineIllegalMoves () =
    let game = {BoardSize = 3; CurrentBoard = [' ';' ';' ';' ';' ';' ';' ';' ';' ']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    Assert.True(isLegalMove (8, game))
    Assert.True(not(isLegalMove (10,game)))
    Assert.True(not(isLegalMove (-1,game)))
    let game2 = {BoardSize = 4; CurrentBoard = [' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}

    Assert.True(isLegalMove (14, game2))

let returnStringNum () =
    "0"

let returnNum (f : unit -> string) =
    let x = returnStringNum()
    x |> System.Int32.Parse

[<Fact>]
let canHumanMakeMove () =
    let game = {BoardSize = 3; CurrentBoard = [' ';' ';' ';' ';' ';' ';' ';' ';' ']; HumanCharacter = 'O'; ComputerCharacter = 'X'; DoesComputerGoFirst = true; TurnNumber = 1}
    Assert.Equal<int>(0,humanMove (moveInput, game, returnStringNum,printHolder))

[<Fact>]
let canComputerMakeMove () =
    let testGame = [' ';' ';' ';' ';' ';' ';' ';' ';' ']
    Assert.Equal<int>(0,computerMove (testGame, true))

[<Fact>]
let canUserInputMove () =
    Assert.Equal<int>(0,moveInput(returnStringNum,printHolder))

[<Fact>]
let canDisplayInvertedBoard() =
    let testGame = ['O';'X';'O';'O';'X';'O';'X';'O';'X']
    Assert.Equal<string>("Current board:\n___________\n|O|X|O|\n___________\n|O|X|O|\n___________\n|X|O|X|\n___________\n",displayBoard(testGame,3,true))

[<Fact>]
let canDisplayUninvertedBoard () =
    let testGame = ['O';'X';'O';'O';'X';'O';'X';'O';'X']
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
let canGetSizeOfBoard () =
    Assert.Equal<int>(4,getSizeOfBoard(printHolder,returnString))