module Types where

data Dir = L | R | U | D deriving (Show)

data Move = Move Dir Int deriving (Show)

type Pos = (Int, Int)

type State = [Pos]