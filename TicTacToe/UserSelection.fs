module UserSelection

let getHumanCharacter (print, input : unit -> string) =
    print "Please enter the character for your moves:\n"
    let userCharacter = input().ToCharArray()
    print "\n"
    userCharacter.[0]


let getComputerCharacter (print, input : unit -> string) =
    print "Please enter the character for the computer's moves:\n"
    let compCharacter = input()
    print "\n"
    compCharacter.[0]


let verifyCharacters (userCharacter, compCharacter) =
    not(userCharacter = ' ' || compCharacter = ' ' || userCharacter = '\r' || compCharacter = '\r' || userCharacter = compCharacter|| userCharacter = '_' || compCharacter = '_' )


let rec getCharacterInput (print : string -> unit, input : unit -> string) : char list=
    let userCharacter = getHumanCharacter(print, input)
    let compCharacter = getComputerCharacter(print, input)
    if(verifyCharacters(userCharacter,compCharacter)) then
        let returnChars = [userCharacter; compCharacter]
        returnChars
    else
        print "Invalid characters.  Please do not use the same character for both, use spaces, the enter key, or underscore.\n"
        getCharacterInput(print, input)



let doesComputerGoFirst (print, input : unit -> string) =
    print "You can go first or the computer can go first.  If you would like to go first please enter '1', otherwise enter any other key.\n"
    let whoGoesFirst = input().ToCharArray()
    print "\n"
    not(whoGoesFirst.[0] = '1')


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

let askIfMinimax (print, input : unit -> string) = 
    print ("If you would like to play against the minimax algorithm press the 1 key.  Press any other key for a rules algorithm.\n")
    let answer = input().ToCharArray()
    answer.[0] = '1'