module Main where

import Data.List (foldl', sortBy)
import Data.List.Split (splitWhen)

solvePartOne :: [Integer] -> Integer
solvePartOne = head

solvePartTwo :: [Integer] -> Integer
solvePartTwo = sum . take 3

main :: IO ()
main = do
  inventories <- splitWhen null . lines <$> readFile "input.txt"
  let sumAsIntegers = foldl' (\acc x -> acc + read x) 0
      calorieSums = map sumAsIntegers inventories
      sortedSums = sortBy (flip compare) calorieSums
  print $ solvePartOne sortedSums
  print $ solvePartTwo sortedSums
