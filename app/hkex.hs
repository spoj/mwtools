module Main where

import Network.Curl
import Text.HTML.TagSoup
import Control.Arrow

firstSection :: (a->Bool) -> [a] -> [a]
firstSection p = sections p >>> head

firstTag :: [a] -> a
firstTag = head

f :: String -> String -> String
f x = parseTags >>>
      firstSection (~== TagOpen "input" [("name",x)]) >>> 
      firstTag >>>
      fromAttrib "value"

ff x s = x ++ "=" ++ f x s 

searchURL = "http://www.hkexnews.hk/listedco/listconews/advancedsearch/search_active_main.aspx" 

opts = [ CurlCookieJar "/tmp/xxcookies" ]

something = do
  (_,s) <- curlGetString searchURL opts
  (_,s2) <- curlGetString searchURL (method_POST ++ opts ++ [CurlPostFields
    [ "ctl00$txt_stock_code=0005"
    , ff "__VIEWSTATE" s
    , ff "ctl00$txt_today" s
    , ff "ctl00$hfStatus" s
    , ff "ctl00$rdo_SelectDocType" s
    , "ctl00$txt_stock_code=00005"
    , "ctl00$sel_tier_1=4"
    , "ctl00%24sel_tier_2=159&"
    ,"ctl00%24txtKeyWord=&"
    , "ctl00%24rdo_SelectDateOfRelease=rbManualRange"
    , "ctl00%24sel_DateOfReleaseFrom_d=01"
    , "ctl00%24sel_DateOfReleaseFrom_m=01"
    , "ctl00%24sel_DateOfReleaseFrom_y=2012"
    , "ctl00%24sel_DateOfReleaseTo_d=04"
    , "ctl00%24sel_DateOfReleaseTo_m=08"
    , "ctl00%24sel_DateOfReleaseTo_y=2013"
    , "ctl00%24sel_defaultDateRange=SevenDays"
    ]])
  writeFile "/tmp/out.txt" s2
