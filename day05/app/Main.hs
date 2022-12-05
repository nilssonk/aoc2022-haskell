{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.ByteString
  ( Parser,
    anyWord8,
    choice,
    endOfInput,
    many1,
    parseOnly,
    string,
    takeWhile1,
    word8,
  )
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (isNumber)
import Data.Either (rights)
import Data.List (foldl')
import qualified Data.Map as M

parseBoxRow :: Parser String
parseBoxRow = do
  result <-
    choice
      [ do
          string "   "
          return [],
        do
          word8 $ c2w '['
          c <- anyWord8
          word8 $ c2w ']'
          return [w2c c]
      ]
  void (word8 $ c2w ' ') <|> endOfInput
  return result

stackParser :: Parser [String]
stackParser = many1 parseBoxRow

moveParser :: Parser (Int, Int, Int)
moveParser = do
  string "move "
  n <- readNum
  string " from "
  a <- readNum
  string " to "
  b <- readNum
  return (n, a, b)
  where
    readNum = read . C.unpack <$> takeWhile1 (isNumber . w2c)

solveGeneric :: (String -> String) -> M.Map Int String -> [(Int, Int, Int)] -> String
solveGeneric f m =
  map (head . snd)
    . M.toList
    . foldl'
      ( \acc (n, from, to) ->
          let values = take n $ acc M.! from
           in M.alter (appendValues (f values)) to $ M.adjust (drop n) from acc
      )
      m
  where
    appendValues xs = \case
      Just old -> Just $ xs ++ old
      Nothing -> Just xs

solvePartOne :: M.Map Int String -> [(Int, Int, Int)] -> String
solvePartOne = solveGeneric reverse

solvePartTwo :: M.Map Int String -> [(Int, Int, Int)] -> String
solvePartTwo = solveGeneric id

main :: IO ()
main = do
  input <- C.lines <$> C.readFile "input.txt"
  let boxRows = rights . map (parseOnly stackParser) $ input
      boxesWithColumns = concatMap (zip [1 ..]) boxRows
  let moves = rights . map (parseOnly moveParser) $ input
      initialState = M.fromListWith (flip (++)) boxesWithColumns
  putStrLn $ solvePartOne initialState moves
  putStrLn $ solvePartTwo initialState moves