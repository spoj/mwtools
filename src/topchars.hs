module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char
import System.Environment (getArgs)

import TopStrings

nFromArgs :: [String] -> Int
nFromArgs [] = 1
nFromArgs (n:_) = read n

comb :: Int -> T.Text -> [T.Text]
comb n = map (T.take n) . T.tails

main :: IO ()
main = do
  n <- fmap nFromArgs getArgs
  TIO.getContents >>= printTopStrings . topStrings . comb n . T.filter isPrint
