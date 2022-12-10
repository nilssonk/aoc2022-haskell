{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal (w2c)
import Data.Char (isDigit)
import Data.Either (fromRight)
import Types (Dir (..), Move (..))

readNum = read . C.unpack <$> AP.takeWhile (isDigit . w2c)

parseRight :: AP.Parser Dir
parseRight = do
  AP.string "R "
  return R

parseLeft :: AP.Parser Dir
parseLeft = do
  AP.string "L "
  return L

parseDown :: AP.Parser Dir
parseDown = do
  AP.string "D "
  return D

parseUp :: AP.Parser Dir
parseUp = do
  AP.string "U "
  return U

parseMove :: AP.Parser Move
parseMove = do
  dir <- parseRight <|> parseLeft <|> parseUp <|> parseDown
  d <- readNum
  AP.takeWhile (AP.inClass " \n")
  return $ Move dir d

parseMoves :: BS.ByteString -> [Move]
parseMoves = fromRight [] . AP.parseOnly (AP.many1 parseMove)