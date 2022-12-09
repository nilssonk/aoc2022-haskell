module Types where

import qualified Data.ByteString as BS

data File = File BS.ByteString Int deriving (Show, Eq)

newtype Dir = Dir BS.ByteString deriving (Show, Eq)

data Node = D Dir | F File deriving (Show, Eq)

type SizedNode = (Node, Int)

data Command = Down Dir | Up | Stat [Node] deriving (Show, Eq)
