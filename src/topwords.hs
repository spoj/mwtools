module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (tails)
import System.Environment (getArgs)
import Data.Char

import TopStrings

nFromArgs :: [String] -> Int
nFromArgs [] = 1
nFromArgs (n:_) = read n

comb :: Int -> [T.Text] -> [T.Text]
comb n = map (T.unwords . take n) . tails

main :: IO ()
main = do
  n <- fmap nFromArgs getArgs
  TIO.getContents >>= printTopStrings . topStrings . comb n . T.words . T.filter isPrint
