module Day1.Main where

import Data.List (sortOn)
import Data.List.Split (splitWhen)
import Data.Ord (Down (Down), comparing)

solve1 :: [[Int]] -> Int
solve1 = maximum . map sum

solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . sortOn Down . map sum

parse :: String -> [[Int]]
parse = map (map read) . splitWhen null . lines

main :: IO ()
main = do
  input <- parse <$> getContents
  print (solve1 input)
  print (solve2 input)
