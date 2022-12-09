module Main where

import qualified Data.Attoparsec.ByteString as AP
import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal (c2w)
import Data.Either (fromRight)
import Data.List.Unique (allUnique)
import Data.Word (Word8)

type ScannerState = (Int, [Word8], [Int])

type Scanner = ScannerState -> Word8 -> Maybe ScannerState

genericScanner :: Int -> Scanner
genericScanner bufSize (n, buf, result) c
  | length buf < bufSize =
      Just
        (n + 1, appendedBuf, result)
  | allUnique buf =
      Just
        (n + 1, shiftedBuf, n : result)
  | otherwise =
      Just
        (n + 1, shiftedBuf, result)
  where
    appendedBuf = c : buf
    shiftedBuf = c : init buf

messageScanner :: Scanner
messageScanner = genericScanner 14

packetScanner :: Scanner
packetScanner = genericScanner 4

scannerResult :: ScannerState -> [Int]
scannerResult (_, _, result) = result

parseWithScanner :: Scanner -> C.ByteString -> [Int]
parseWithScanner scanner = resultOrDefault . doScan
  where
    resultOrDefault = fromRight [] . second (scannerResult . snd)
    doScan = AP.parseOnly (AP.runScanner (0, [], []) scanner)

main :: IO ()
main = do
  input <- C.filter (/= '\n') <$> C.readFile "input.txt"
  let partOne = last $ parseWithScanner packetScanner input
  print partOne
  let partTwo = last $ parseWithScanner messageScanner input
  print partTwo
