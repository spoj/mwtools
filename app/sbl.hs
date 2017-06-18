module Main(main) where

import Data.List(sortBy)
import Data.Function(on)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn op = sortBy (compare `on` op)

-- The main feature of this program. Sort by length of each line
sbl :: [[a]] -> [[a]]
sbl = sortOn length

main :: IO ()
main = getContents >>= mapM_ putStrLn . sbl . lines
