{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.ByteString.Char8 as C
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Tree as T
import Parsing (parseCommands)
import TreeBuilder (buildTree)
import Types (Command (..), Dir (Dir), File (File), Node (..), SizedNode)

solvePartOne :: Maybe (T.Tree SizedNode) -> Int
solvePartOne =
  maybe
    0
    ( T.foldTree
        (\(x, size) xs -> if size <= 100000 then size + sum xs else sum xs)
    )

solvePartTwo :: Maybe (T.Tree SizedNode) -> Int
solvePartTwo t =
  minimum . catMaybes $
    maybe
      []
      ( T.foldTree
          ( \(_, size) xs ->
              let candidate = if totalSize - size <= maxSize then Just size else Nothing
               in candidate : concat xs
          )
      )
      t
  where
    maxSize = 70000000 - 30000000
    totalSize = maybe 0 (snd . T.rootLabel) t

makeDirTree :: [Command] -> Maybe (T.Tree (Node, Int))
makeDirTree =
  treeFilter
    ( \case
        (D _, size) -> True
        (F _, _) -> False
    )
    . buildTree
  where
    treeFilter f (T.Node x children) =
      if f x
        then Just $ T.Node x (mapMaybe (treeFilter f) children)
        else Nothing

main :: IO ()
main = do
  input <- C.readFile "input.txt"
  let commands = parseCommands input
      tree = makeDirTree commands
  print $ solvePartOne tree
  print $ solvePartTwo tree
