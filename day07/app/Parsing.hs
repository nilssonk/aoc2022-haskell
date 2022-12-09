{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
  ( Parser,
    inClass,
    many',
    parseOnly,
    skip,
    string,
    takeWhile1,
    word8,
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (isNumber)
import Data.Either (fromRight)
import Types (Command (..), Dir (Dir), File (File), Node (..))

parseMove :: Parser Command
parseMove = do
  string "$ cd "
  arg <- takeWhile1 (/= c2w '\n')
  skip (inClass " \n")
  return
    ( case arg of
        ".." -> Up
        dir -> Down (Dir dir)
    )

parseDir :: Parser Node
parseDir = do
  string "dir "
  arg <- takeWhile1 (/= c2w '\n')
  skip (inClass " \n")
  return $ D (Dir arg)

parseFile :: Parser Node
parseFile = do
  size <- readInt <$> takeWhile1 (isNumber . w2c)
  word8 (c2w ' ')
  name <- takeWhile1 (/= c2w '\n')
  skip (inClass " \n")
  return $ F (File name size)
  where
    readInt = read . C.unpack

parseEntries :: Parser [Node]
parseEntries = many' (parseDir <|> parseFile)

parseStat :: Parser Command
parseStat = do
  string "$ ls"
  skip (inClass " \n")
  Stat <$> parseEntries

parseCommands :: BS.ByteString -> [Command]
parseCommands = fromRight [] . parseOnly (many' (parseMove <|> parseStat))