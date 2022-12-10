module Main where

import Data.Char (digitToInt)
import Data.Matrix
  ( Matrix (ncols, nrows),
    fromLists,
    getCol,
    getRow,
  )
import qualified Data.Set as S
import qualified Data.Vector as V

visibleCheck = visibleCheck' []
  where
    visibleCheck' xs (y@(_, h1) : ys)
      | all (\(_, h2) -> h1 > h2) ys = visibleCheck' (y : xs) ys
      | otherwise = visibleCheck' xs ys
    visibleCheck' xs [] = xs

visibleTrees mat =
  let width = ncols mat
      height = nrows mat
      rows = [1 .. height]
      cols = [1 .. width]
   in S.fromList $
        concatMap
          ( \r ->
              let rowData = zip [1 ..] . V.toList $ getRow r mat
                  visibleLeft = visibleCheck rowData
                  visibleRight = visibleCheck $ reverse rowData
               in [(x, r) | x <- map fst $ visibleLeft ++ visibleRight]
          )
          rows
          ++ concatMap
            ( \c ->
                let colData = zip [1 ..] . V.toList $ getCol c mat
                    visibleTop = visibleCheck colData
                    visibleBottom = visibleCheck $ reverse colData
                 in [(c, y) | y <- map fst $ visibleTop ++ visibleBottom]
            )
            cols

scenicScore :: (Ord a, Num t) => [a] -> t
scenicScore (x : xs) = scenicScore' 0 [x] xs
  where
    scenicScore' n (x : xs) (y : ys)
      | x > y = scenicScore' (n + 1) (x : y : xs) ys
      | otherwise = n + 1
    scenicScore' n _ _ = n
scenicScore [] = 0

scenics :: Matrix Int -> [Int]
scenics mat =
  let width = ncols mat
      height = nrows mat
      rows = [0 .. height - 1]
      cols = [0 .. width - 1]
   in [ let rowData = getRow (y + 1) mat
            colData = getCol (x + 1) mat
            left = map (rowData V.!) [x, x - 1 .. 0]
            right = map (rowData V.!) [x .. width - 1]
            up = map (colData V.!) [y, y - 1 .. 0]
            down = map (colData V.!) [y .. height - 1]
         in product $ map scenicScore [left, right, up, down]
        | x <- cols,
          y <- rows
      ]

main :: IO ()
main = do
  input <- map (map digitToInt) . lines <$> readFile "input.txt"
  let heightMap = fromLists input
      partOne = S.size $ visibleTrees heightMap
  print partOne
  let partTwo = maximum $ scenics heightMap
  print partTwo
