module UserSelection
open Game

let getPlayer1Character (print, input : unit -> string) =
    print "Please enter the character that will symbolize the moves for player 1:\n"
    let userCharacter = input().ToCharArray()
    print "\n"
    userCharacter.[0]


let getPlayer2Character (print, input : unit -> string) =
    print "Please enter the character that will symbolize the moves for player 2:\n"
    let compCharacter = input()
    print "\n"
    compCharacter.[0]


let verifyCharacters (userCharacter, compCharacter) =
    not(userCharacter = ' ' || compCharacter = ' ' || userCharacter = '\r' || compCharacter = '\r' || userCharacter = compCharacter|| userCharacter = '_' || compCharacter = '_' )


let rec getCharacterInput (print : string -> unit, input : unit -> string) : char list=
    let userCharacter = getPlayer1Character(print, input)
    let compCharacter = getPlayer2Character(print, input)
    if(verifyCharacters(userCharacter,compCharacter)) then
        let returnChars = [userCharacter; compCharacter]
        returnChars
    else
        print "Invalid characters.  Please do not use the same character for both, use spaces, the enter key, or underscore.\n"
        getCharacterInput(print, input)


let getSizeOfBoard (print, input : unit -> string) =
    print "Would you like to play on a 3X3 board or 4X4 board? Enter '3' for 3X3.\n"
    let boardSize = input().ToCharArray()
    if boardSize.[0] = '3' then
        3
    else
        4

let askIfGameOver (print, input : unit -> string) = 
    print ("If you would like to play again please press the Y key.\n")
    let answer = input().ToCharArray()
    answer.[0] = 'y' || answer.[0] = 'Y'

let askIfInverted (print, input : unit -> string) = 
    print ("If you would like to have biggest number on top of input press the 1 key.\n")
    let answer = input().ToCharArray()
    answer.[0] = '1'

let rec askAlgorithmType (print, input : unit -> string, gameSize) = 
    print ("Please select an algorithm to play against:\n")
    print ("1. minimax\n")
    print ("2. stupid\n")
    if(gameSize = 3 || gameSize = 4) then
        print ("3. rules\n")

    let answer = input()
    if(answer = "1" || answer = "2") then
        answer |> System.Int32.Parse
    else
        print ("Invalid input. \n")
        askAlgorithmType (print, input, gameSize)

let rec askForPlayerInformation (print, input : unit -> string, player, gameSize) =
    print ("Please select what type of player Player " + player.ToString() + " is: \n")
    print ("1. Human\n")
    print ("2. Computer\n")
    let answer = input()
    if answer = "1" then
        let seed = answer |> System.Int32.Parse
        {PlayerSeed = seed; ComputerAlgorithm = 0; PlayerCharacter = ' '}
    elif answer = "2" then
        let seed = answer |> System.Int32.Parse
        let algorithm = askAlgorithmType(print,input,gameSize)
        {PlayerSeed = seed; ComputerAlgorithm = algorithm; PlayerCharacter = ' '}
    else
        print ("Invalid input. \n")
        askForPlayerInformation (print, input, player,gameSize)
