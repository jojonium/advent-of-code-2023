module Day02 (main) where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

data Balls = Balls
  { red   :: Int
  , green :: Int
  , blue  :: Int
  } deriving (Show)

data Game = Game
  { id    :: Int
  , balls :: [Balls]
  } deriving (Show)

parse :: String -> Game
parse str = case splitOn ":" str of
  (a:b:_) -> Game gameID draws
    where gameID = read $ words a !! 1
          draws  = map parseBalls (splitOn ";" b)
  _       -> error $ "Invalid string " ++ str

parseBalls :: String -> Balls
parseBalls str = Balls numRed numGreen numBlue
  where clauses    = splitOn "," str
        defaultMap = Map.fromList [("red", 0), ("green", 0), ("blue", 0)]
        colors     = foldl helper defaultMap clauses
        helper m s = case words s of
          (n:c:_) -> Map.insert c (read n) m
          _       -> error $ "Invalid clause " ++ s
        numRed     = colors Map.! "red"
        numGreen   = colors Map.! "green"
        numBlue    = colors Map.! "blue"

part1 :: Game -> Int
part1 (Game i bs)
  | all (\(Balls r g b) -> r <= 12 && g <= 13 && b <= 14) bs = i
  | otherwise = 0

main :: IO ()
main = do
  games <- map parse . lines <$> getInput "inputs/day02.txt"
  putStrLn $ "Part 1: " ++ show (sum (map part1 games))

