module ConsoleTasks

let rec moveInput (moveEntered, print) = 
    print ("Please select your move: ")
    let input = moveEntered()
    try
        input |> System.Int32.Parse
    with
        | :? System.FormatException -> moveInput(moveEntered, print)


let displayInvertedBoard (board : char list, boardSize) =
    let mutable x = ""
    for i in 0 .. boardSize-1 do
        x <-  x + "___________\n"
        x <- x + "|"
        for j in 0 .. boardSize-1 do
            x <- x + board.[(i*boardSize) + j].ToString()
            x <- x + "|"
        x <- x + "\n"
    x <- x + "___________\n"
    x


let displayUninvertedBoard (board : char list, boardSize) =
    let mutable x = ""
    for i in boardSize-1 .. -1 .. 0 do
        x <-  x + "___________\n"
        x <- x + "|"
        for j in boardSize-1 .. -1 .. 0 do
            x <- x + board.[(i*boardSize) + j].ToString()
            x <- x + "|"
        x <- x + "\n"
    x <- x + "___________\n"
    x

let displayBoard (board : char list, boardSize, isInverted) =
    if(isInverted) then
        "Current board:\n" + displayInvertedBoard(board,boardSize)
    else
        "Current board:\n" + displayUninvertedBoard(board,boardSize)


let displayInvertedBoardOptions (boardSize) =
    let mutable x = ""
    for i in 0 .. boardSize-1 do
        x <- x + "___________\n"
        x <- x + "|"
        for j in 0 .. boardSize-1 do
            x <- x + ((i*boardSize) + j).ToString()
            x <- x + "|"
        x <- x + "\n"
    x <- x + "___________\n"
    x

let displayUnivertedBoardOptions (boardSize) =
    let mutable x = ""
    for i in boardSize-1 .. -1 .. 0 do
        x <- x + "___________\n"
        printf "|"
        for j in boardSize-1 .. -1 .. 0 do
            x <- x + ((i*boardSize) + j).ToString()
            x <- x + "|"
        x <- x + "\n"
    x <- x + "___________\n"
    x

let displayBoardOptions (boardSize, isInverted) =
    if(isInverted) then
        "Board input:\n" + displayInvertedBoardOptions(boardSize)
    else
        "Board input:\n" + displayUnivertedBoardOptions (boardSize)