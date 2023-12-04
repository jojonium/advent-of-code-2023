module Day04 (main) where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (intersect)
import qualified Data.Map as Map

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
  putStrLn $ "Part 1: " ++ show (sum (map score1 cards))
  putStrLn $ "Part 2: " ++ show (part2 cards)

parse :: String -> Card
parse s = Card num w h
  where num    = read (take 3 (drop 5 s))
        (w, h) = case map (map read . words) (splitOn " | " (drop 10 s)) of
                   (a:b:_) -> (a, b)
                   _       -> error "Invalid line"

score1 :: Card -> Int
score1 (Card _ winning have)
  | matches > 0 = 2 ^ (matches - 1)
  | otherwise   = 0
  where matches = length (winning `intersect` have)

part2  :: [Card] -> Int
part2 cards = score2 cards initMap (length cards)
  where initMap = Map.fromList (map (\c -> (_id c, 1)) cards)

score2 :: [Card] -> Map.Map Int Int -> Int -> Int
score2 [] m _ = Map.foldr (+) 0 m
score2 ((Card cid winning have):cards) m end = score2 cards m' end
  where matches = length (winning `intersect` have)
        copies  = m Map.! cid
        m'      = foldl f m [(cid + 1) .. (min (cid + matches) end)]
        f ts i  = Map.insertWith (+) i copies ts
