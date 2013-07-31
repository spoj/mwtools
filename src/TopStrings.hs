{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module TopStrings where

import Prelude hiding (mapM_, foldr)
import Data.Foldable
import Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as IM

type FreqMap a = M.HashMap a Int
type CoFreqMap a = IM.IntMap [a]

freqMap :: (Eq a, Hashable a) => [a] -> FreqMap a
freqMap = foldl' mapIncrement M.empty

mapIncrement :: (Eq a, Hashable a) => FreqMap a -> a -> FreqMap a
mapIncrement m a = M.insertWith f a 1 m
  where f new old = new + old

coFreqMap :: FreqMap a -> CoFreqMap a
coFreqMap = M.foldrWithKey f IM.empty
  where f a freq im = IM.insertWith (++) freq [a] im

freqList :: CoFreqMap a -> [(Int,[a])]
freqList = IM.toAscList

topStrings :: [T.Text] -> [(Int,[T.Text])]
topStrings = freqList . coFreqMap . freqMap

printTopStrings :: [(Int,[T.Text])] -> IO ()
printTopStrings =
  mapM_ (\(a,bs) -> mapM_ (\b -> putStr (show a) >> putStr "\t" >> TIO.putStrLn b) bs)
