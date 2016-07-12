module Game

type Game = 
    {   BoardSize : int 
        CurrentBoard : char option list 
        TurnNumber : int}

type Player =
    {   PlayerSeed : int
        ComputerAlgorithm : int
        PlayerCharacter: char}