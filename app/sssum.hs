module Main where
import SubsetSum
import Text.Printf

main :: IO ()
main = getContents
  >>= printoutput
    . subsetsum'
    . map read
    . takeWhile (/=".")
    . lines
    . filter (`elem`"0123456789-.\n")

printoutput Nothing = putStrLn "Nothing"
printoutput (Just l) = do
  printf "%2f\n" $ sum l
  mapM_ (printf "%2f\n") l
  return ()
