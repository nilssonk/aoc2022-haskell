{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Maybe (mapMaybe)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Text.Regex.PCRE.Light (compile, match, utf8)

type PairSet = (Int, Int, Int, Int)

matchTuple :: String -> Maybe [ByteString]
matchTuple x = match (compile "(\\d+)-(\\d+),(\\d+)-(\\d+)" [utf8]) (fromString x) []

tuple4 :: [ByteString] -> PairSet
tuple4 xs = (doRead 1, doRead 2, doRead 3, doRead 4)
  where
    doRead = read . unpack . decodeUtf8 . (!!) xs

contains :: PairSet -> Bool
contains (a, b, c, d) = (a <= c && b >= d) || (c <= a && d >= b)

overlaps :: PairSet -> Bool
overlaps x@(a, b, c, d) = (a >= c && a <= d) || (b >= c && b <= d) || contains x

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  let matches = mapMaybe matchTuple ls
      tuples = map tuple4 matches
      partOne = length $ filter contains tuples
  print partOne
  let partTwo = length $ filter overlaps tuples
  print partTwo