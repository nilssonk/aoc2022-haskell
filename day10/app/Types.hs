module Types where

data Op = Noop | AddX Int deriving (Show)

data TimedOp = TimedOp Int Op deriving (Show)