module Day3.Main where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Char (isLower, ord)
import Data.List.Split (chunksOf)
import Data.Set (Set, fromList, intersection, toList)

priority :: Char -> Int
priority char =
  ord char
    + if isLower char
      then -ord 'a' + 1
      else -ord 'A' + 27

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

solve1 :: [[Char]] -> Int
solve1 = sum . map (priority . uncurry findRepetition . divideRucksack)
  where
    divideRucksack :: [Char] -> (Set Char, Set Char)
    divideRucksack = join bimap fromList . splitHalf
    findRepetition set1 set2 = (head . toList) (intersection set1 set2)

solve2 :: [[Char]] -> Int
solve2 = sum . map (priority . findBadge) . chunksOf 3 . map fromList
  where
    findBadge :: [Set Char] -> Char
    findBadge = head . toList . foldr1 intersection

parse :: String -> [[Char]]
parse = lines

main :: IO ()
main = do
  input <- parse <$> getContents
  print (solve1 input)
  print (solve2 input)
