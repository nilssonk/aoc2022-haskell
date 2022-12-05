module Main where

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print input
