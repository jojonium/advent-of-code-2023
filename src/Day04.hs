module Day04 (main) where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (intersect)

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

data Card = Card
  { _id      :: Int
  , _winning :: [Int]
  , _have    :: [Int]
  } deriving Show

main :: IO ()
main = do
  cards <- map parse . lines <$> getInput "inputs/day04.txt"
  putStrLn $ "Part 1: " ++ show (sum (map score cards))

parse :: String -> Card
parse s = Card num w h
  where num    = read (take 3 (drop 5 s))
        (w, h) = case map (map read . words) (splitOn " | " (drop 10 s)) of
                   (a:b:_) -> (a, b)
                   _       -> error "Invalid line"

score :: Card -> Int
score (Card _ winning have)
  | matches > 0 = 2 ^ (matches - 1)
  | otherwise   = 0
  where matches = length (winning `intersect` have)
