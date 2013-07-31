module Main where

import Data.Char(isSpace)
import Data.List(groupBy)
import Data.Function (on)

trim :: String -> String
trim = trimTail . trimHead 

trimHead,trimTail :: String -> String
trimHead = dropWhile isSpace
trimTail = takeWhile (not.isSpace)

f :: String -> String
f = unlines . map unwords . groupBy ((==) `on` null) . map trim . lines

main :: IO ()
main = interact f
