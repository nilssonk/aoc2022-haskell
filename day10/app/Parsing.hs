{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal (w2c)
import Data.Char (isDigit)
import Data.Either (fromRight)
import Types (Op (AddX, Noop), TimedOp (TimedOp))

readNum :: AP.Parser Int
readNum = read . C.unpack <$> AP.takeWhile (\c -> isDigit (w2c c) || w2c c == '-')

parseNoop :: AP.Parser TimedOp
parseNoop = do
  AP.string "noop"
  return $ TimedOp 1 Noop

parseAddX :: AP.Parser TimedOp
parseAddX = do
  AP.string "addx "
  TimedOp 2 . AddX <$> readNum

parseOp :: AP.Parser TimedOp
parseOp = do
  op <- parseNoop <|> parseAddX
  AP.takeWhile (AP.inClass " \n")
  return op

parseOps :: C.ByteString -> [TimedOp]
parseOps = fromRight [] . AP.parseOnly (AP.many1 parseOp)