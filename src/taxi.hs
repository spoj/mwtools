module Main (main) where

import System.Environment (getArgs)

main = do
  [x,d] <- getArgs
  print $ case x of
    "red" -> red (read d)
    "green" -> green (read d)

type Distance = Double
type Price = Double

red :: Distance -> Price
red d
  | d <= 2 = 20
  | d <= 9 = d*7.5+5
  | otherwise = d*5+27.5

green :: Distance -> Price
green d
  | d <= 2 = 16.5
  | d <= 8 = 6.5 * d + 3.5
  | otherwise = d * 5 + 15.5
