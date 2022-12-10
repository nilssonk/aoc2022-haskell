module Main where

import qualified Data.ByteString.Char8 as C
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Parsing (parseOps)
import Types (Op (AddX, Noop), TimedOp (TimedOp))

execOne :: (Int, Int) -> TimedOp -> (Int, Int)
execOne (cycle, x) (TimedOp c op) = (cycle + c, newX)
  where
    newX = case op of
      Noop -> x
      AddX dx -> x + dx

execAll :: [TimedOp] -> [(Int, Int)]
execAll =
  reverse
    . foldl'
      ( \xs@(x : _) op -> execOne x op : xs
      )
      [(1, 1)]

stateAtCycle :: [(Int, Int)] -> Int -> (Int, Int)
stateAtCycle xs c = (c, x)
  where
    x = snd . closestToCycles c $ xs
    closestToCycles c1 = last . takeWhile (\(c2, _) -> c1 >= c2)

main :: IO ()
main = do
  input <- C.readFile "input.txt"
  let ops = parseOps input
      states = execAll ops
      partOne = sum . map (uncurry (*) . stateAtCycle states) $ [20, 60, 100, 140, 180, 220]
  print partOne
  let totalCycles = sum . map (\(TimedOp c _) -> c) $ ops
      lineWidth = 40
      processPixel = pixelValue . isVisible . stateAtCycle states
        where
          pixelValue v = if v then '#' else '.'
          isVisible (c, x) = x - 1 <= cursorX && cursorX <= x + 1
            where
              cursorX = (c - 1) `rem` lineWidth
      partTwo = unlines . chunksOf lineWidth . map processPixel $ [1 .. totalCycles]
  putStrLn partTwo