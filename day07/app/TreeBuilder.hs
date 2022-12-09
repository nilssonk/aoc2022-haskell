{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeBuilder where

import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Tree as T
import qualified Data.Tree.Zipper as Z
import Types (Command (..), Dir (Dir), File (File), Node (..), SizedNode)

type TreePos = Z.TreePos Z.Full SizedNode

applyCommand :: TreePos -> Command -> TreePos
applyCommand pos Up = fromMaybe pos (Z.parent pos)
applyCommand pos (Down dir) =
  case findChild (D dir) (Z.firstChild pos) of
    Just child -> child
    Nothing -> Z.insert (T.Node (D dir, 0) []) $ Z.children pos
  where
    findChild dir next = do
      node <- next
      if fst (Z.label node) == dir
        then next
        else findChild dir (Z.next node)
applyCommand pos (Stat fs) =
  Z.modifyTree
    (\(T.Node x children) -> T.Node x (mapMaybe makeNode fs ++ children))
    pos
  where
    makeNode f = case f of
      file@(F (File _ size)) -> Just (T.Node (file, size) [])
      _ -> Nothing

sumDirs :: T.Tree SizedNode -> T.Tree SizedNode
sumDirs = \case
  T.Node (D dir, _) children -> T.Node (D dir, totalSize) newChildren
    where
      newChildren = map sumDirs children
      totalSize = sum $ map (snd . T.rootLabel) newChildren
  n -> n

buildTree :: [Command] -> T.Tree SizedNode
buildTree = sumDirs . Z.toTree . Z.root . foldl' applyCommand initial
  where
    initial = Z.fromTree (T.Node (D $ Dir "root", 0) [])