module Main where

import Data.Char (ord)
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)

priority :: Char -> Int
priority x
  | value >= ord 'a' = value - ord 'a' + 1
  | otherwise = value - ord 'A' + 27
  where
    value = ord x

sharedItem :: [String] -> Maybe Char
sharedItem [] = Nothing
sharedItem ((x : xs) : ys)
  | elemOfAll ys = Just x
  | otherwise = sharedItem (xs : ys)
  where
    elemOfAll = all (x `elem`)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let packHalves = map (\line -> chunksOf (length line `div` 2) line) input
      packShared = mapMaybe sharedItem packHalves
      partOne = sum $ map priority packShared
  print partOne
  let groups = chunksOf 3 input
      groupShared = mapMaybe sharedItem groups
      partTwo = sum $ map priority groupShared
  print partTwo