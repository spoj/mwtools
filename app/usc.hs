module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import TopStrings

main :: IO ()
main = TIO.getContents >>= printTopStrings . topStrings . T.lines
