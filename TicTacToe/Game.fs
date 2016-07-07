module Game

type Game = 
    {   BoardSize : int 
        CurrentBoard : char list 
        HumanCharacter : char
        ComputerCharacter: char
        DoesComputerGoFirst: bool
        TurnNumber : int}