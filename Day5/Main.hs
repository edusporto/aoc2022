module Day5.Main where

import Data.Char (isNumber)
import Data.List (transpose)
import Data.List.Extra (splitOn)

type Stack = [Char]

data Move = Move {qty :: Int, from :: Int, to :: Int}
  deriving (Show)

move :: Move -> [Stack] -> [Stack]
move = undefined

parse :: String -> ([Stack], [Move])
parse text = (parseStacks stacksText, map parseMove (lines movesText))
  where
    stacksText : movesText : _ = splitOn "\n\n" text

parseStacks :: String -> [Stack]
parseStacks =
  map (filter (/= ' '))
    . filter (not . blank)
    . transpose
    . init
    . lines
    . map unBracket
  where
    unBracket '[' = ' '
    unBracket ']' = ' '
    unBracket x = x
    blank = all (== ' ')

parseMove :: String -> Move
parseMove text = Move qty from to
  where
    qty : from : to : _ = map read . filter (all isNumber) . splitOn " " $ text

main :: IO ()
main = do
  input <- parse <$> getContents
  print input
