module Main where

import qualified Data.ByteString.Char8 as C
import Data.List (foldl')
import qualified Data.Set as S
import Parsing (parseMoves)
import Types (Dir (D, L, R, U), Move (..), State)

fixTail :: State -> State
fixTail = fixTail' []
  where
    fixTail' as [] = reverse as
    fixTail' [] (b : bs) = fixTail' [b] bs
    fixTail'
      as@((x1, y1) : _)
      ((x2, y2) : bs) = fixTail' ((newX, newY) : as) bs
        where
          dx = x1 - x2
          dy = y1 - y2
          newX
            | abs dx > 1 = x2 + signum dx
            | dx /= 0 && abs dy > 1 = x1
            | otherwise = x2
          newY
            | abs dy > 1 = y2 + signum dy
            | dy /= 0 && abs dx > 1 = y1
            | otherwise = y2

move :: State -> Move -> [State]
move [] _ = []
move as (Move dir d) =
  reverse
    . take d
    . drop 1
    . iterate
      (\(x : xs) -> fixTail $ f x : xs)
    $ as
  where
    f = case dir of
      L -> \(i, j) -> (i - 1, j)
      R -> \(i, j) -> (i + 1, j)
      U -> \(i, j) -> (i, j - 1)
      D -> \(i, j) -> (i, j + 1)

main :: IO ()
main = do
  input <- C.readFile "input.txt"
  let moves = parseMoves input
      performMoves initial = foldl' (\xs@(x : _) m -> move x m ++ xs) initial moves
      uniquePosOfTail = S.size . S.fromList . map last
      partOne = uniquePosOfTail $ performMoves [replicate 2 (0, 0)]
  print partOne
  let partTwo = uniquePosOfTail $ performMoves [replicate 10 (0, 0)]
  print partTwo