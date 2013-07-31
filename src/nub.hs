module Main where
import Data.List(nub)
main = interact (unlines . nub . lines)
