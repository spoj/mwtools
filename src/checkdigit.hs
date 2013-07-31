module Main (main) where
import Data.Char (ord, toUpper, isDigit, isAlpha)
import Control.Monad (guard)
import System.Environment (getArgs)

f >| g = g . f

sumProduct :: Num a => [a] -> [a] -> a
sumProduct a b = sum $ zipWith (*) a b

hkidV :: String -> Bool
--hkidC :: String -> String
hkidV s = mod (sumProduct [8,7..] $ map numValue s) 11 == 0
numValue c
  | isDigit c = read [c]
  | isAlpha c = ord (toUpper c) - ord 'A' + 1

hkidC j = head $ do
  choice <- ['0'..'9'] ++ "A"
  let x = j ++ [choice]
  guard $ hkidV x
  return x

hkidCV s
  | length s == 7 = hkidC s
  | otherwise = show $ hkidV s

main = getArgs >>= head >| hkidCV >| putStrLn
