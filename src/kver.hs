{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Main(main) where

import Network.Curl
import Text.HTML.TagSoup
import Control.Arrow 

matchStrings :: [String]
matchStrings = ["mainline:","stable:"]

korg :: IO String
korg = fmap snd $ curlGetString "https://www.kernel.org/" []

firstSection :: (a->Bool) -> [a] -> [a]
firstSection p = sections p >>> head

firstTag :: [a] -> a
firstTag = head

ver :: String -> String -> String
ver siteText matchString = f siteText
  where f = parseTags >>>
            firstSection (~== matchString) >>>
            firstSection (~==TagOpen "strong" []) >>> 
            firstSection (~== TagText "") >>>
            firstTag >>> fromTagText

vers :: IO [String]
vers = do
  siteText <- korg
  return $ map (ver siteText) matchStrings

main :: IO ()
main = vers >>= print
