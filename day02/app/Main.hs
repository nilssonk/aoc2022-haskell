module Main where

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Tuple.Sequence (sequenceT)

data Choice = Rock | Paper | Scissors deriving (Eq)

superiorChoice :: Choice -> Choice
superiorChoice x = case x of
  Rock -> Paper
  Paper -> Scissors
  Scissors -> Rock

score :: (Choice, Choice) -> Integer
score (x, y) =
  scoreOf y + outcome
  where
    outcome
      | y == superiorChoice x = 6
      | y == x = 3
      | otherwise = 0
    scoreOf x = case x of
      Rock -> 1
      Paper -> 2
      Scissors -> 3

parseLeft :: String -> Maybe Choice
parseLeft x = case x of
  "A" -> Just Rock
  "B" -> Just Paper
  "C" -> Just Scissors
  _ -> Nothing

partOneParser :: (String, String) -> Maybe (Choice, Choice)
partOneParser (x, y) =
  sequenceT
    ( parseLeft x,
      case y of
        "X" -> Just Rock
        "Y" -> Just Paper
        "Z" -> Just Scissors
        _ -> Nothing
    )

partTwoParser :: (String, String) -> Maybe (Choice, Choice)
partTwoParser (x, y) =
  sequenceT
    ( left,
      case y of
        "X" -> fmap inferiorChoice left
        "Y" -> left
        "Z" -> fmap superiorChoice left
        _ -> Nothing
    )
  where
    left = parseLeft x
    inferiorChoice = superiorChoice . superiorChoice

tuple2 :: [a] -> Maybe (a, a)
tuple2 [a, b] = Just (a, b)
tuple2 _ = Nothing

main :: IO ()
main = do
  stringPairs <- mapMaybe (tuple2 . words) . lines <$> readFile "input.txt"
  let firstGame = mapMaybe partOneParser stringPairs
      firstScore = foldl' (\acc x -> acc + score x) 0 firstGame
  print firstScore
  let secondGame = mapMaybe partTwoParser stringPairs
      secondScore = foldl' (\acc x -> acc + score x) 0 secondGame
  print secondScore