{-# LANGUAGE InstanceSigs #-}

module Day4.Main where

import Control.Applicative (Applicative (liftA2))
import Data.List.Extra (chunksOf)
import Data.List.Split (splitWhen)
import Data.Tuple.Extra (both)

data Range a = Range a a

contains :: Ord a => Range a -> Range a -> Bool
contains (Range start1 end1) (Range start2 end2) =
  start1 <= start2 && end1 <= end2

overlaps :: Ord a => Range a -> Range a -> Bool
overlaps (Range start1 end1) (Range start2 end2) =
  start1 <= end2 && start2 <= end1

instance Read a => Read (Range a) where
  readsPrec :: Read a => Int -> ReadS (Range a)
  readsPrec _ input =
    let (v1, _ : v2) = span (/= '-') input
     in [(Range (read v1) (read v2), "")]

instance Show a => Show (Range a) where
  show :: Show a => Range a -> String
  show (Range v1 v2) = show v1 ++ "-" ++ show v2

-- >>> read "10-20" :: Range Int
-- 10-20

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

solve1 :: [(Range Int, Range Int)] -> Int
solve1 = sum . map (boolToInt . uncurry oneContainsOther)
  where
    oneContainsOther r1 r2 = r1 `contains` r2 || r2 `contains` r1

-- for fun: we can implement `oneContainsOther` using `liftA2`
-- oneContainsOther r1 = liftA2 (||) (r1 `contains`) (`contains` r1)

solve2 :: [(Range Int, Range Int)] -> Int
solve2 = sum . map (boolToInt . uncurry overlaps)

parse :: String -> [(Range Int, Range Int)]
parse = map (both read . mkPair . splitWhen (== ',')) . lines
  where
    mkPair :: [a] -> (a, a)
    mkPair l = (head l, l !! 1)

main :: IO ()
main = do
  input <- parse <$> getContents
  print (solve1 input)
  print (solve2 input)
