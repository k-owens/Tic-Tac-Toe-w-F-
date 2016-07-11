module Game

type Game = 
    {   BoardSize : int 
        CurrentBoard : char option list 
        HumanCharacter : char
        ComputerCharacter: char
        DoesComputerGoFirst: bool
        TurnNumber : int}