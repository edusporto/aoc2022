module Day2.Main where

import Data.Bifunctor (second)

data Shape = Rock | Paper | Scissors
  deriving (Eq, Show)

data Result = Win | Draw | Lose
  deriving (Eq, Show)

win :: Shape -> Shape
win them = case them of
  Rock -> Paper
  Paper -> Scissors
  Scissors -> Rock

lose :: Shape -> Shape
lose them = case them of
  Rock -> Scissors
  Paper -> Rock
  Scissors -> Paper

battle :: Shape -> Shape -> Result
battle them me
  | win them == me = Win
  | lose them == me = Lose
  | otherwise = Draw

score :: Shape -> Shape -> Int
score them me =
  reward me + case battle them me of
    Lose -> 0
    Draw -> 3
    Win -> 6
  where
    reward Rock = 1
    reward Paper = 2
    reward Scissors = 3

solve1 :: [(Shape, Char)] -> Int
solve1 = sum . map (uncurry score . second decode)
  where
    decode 'X' = Rock
    decode 'Y' = Paper
    decode 'Z' = Scissors
    decode _ = undefined

solve2 :: [(Shape, Char)] -> Int
solve2 = sum . map (uncurry score . (\(them, code) -> (them, decode them code)))
  where
    decode them code = case code of
      'X' -> lose them
      'Y' -> them
      'Z' -> win them
      _ -> undefined

parse :: String -> [(Shape, Char)]
parse str =
  let ls = lines str
   in map (\ln -> (intoShape (head ln), ln !! 2)) ls
  where
    intoShape 'A' = Rock
    intoShape 'B' = Paper
    intoShape 'C' = Scissors
    intoShape _ = undefined

main :: IO ()
main = do
  input <- parse <$> getContents
  print (solve1 input)
  print (solve2 input)
