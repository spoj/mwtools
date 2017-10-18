module SubsetSum (subsetsum', subsetsum) where
import Control.Monad
import Control.Parallel
import Data.List

subsetsum' :: [Double] -> Maybe [Double]
subsetsum' (a:as) = subsetsum 1 a (reverse $ sort as)
subsetsum' [] = Nothing

subsetsum :: Double -> Double -> [Double] -> Maybe [Double]
subsetsum e n _ | abs n < e = Just []
subsetsum _ _ [] = Nothing
subsetsum e n xxs@(x:xs)
  | sum (filter (>0) xxs) < n-e = Nothing
  | sum (filter (<0) xxs) > n+e = Nothing
  | otherwise = (l`mplus`r)
  where
    l = fmap (x:) (subsetsum e (n-x) xs)
    r = subsetsum e n xs

