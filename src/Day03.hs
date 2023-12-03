module Day03 (main) where

import System.Environment (getArgs)
import Data.Char (isDigit)
import qualified Data.Map as Map
import Debug.Trace (traceShow)

type Coord = (Int, Int)
type Chart = Map.Map Coord Char

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  input <- lines <$> getInput "inputs/day03.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  -- putStrLn $ "Part 2: " ++ show (sum (map part2 games))

parse :: [String] -> Int -> Int -> Chart
parse ls w h = foldr folder Map.empty [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. w - 1]
        ys = [0 .. h - 1]
        folder (x, y) = Map.insert (x, y) p
          where p = ls !! y !! x

part1 :: [String] -> Int
part1 ls = foldr folder 0 [(x, y, chart Map.! (x, y)) | x <- xs, y <- ys]
  where w     = length (head ls)
        h     = length ls
        chart = parse ls w h
        xs    = [0 .. w - 1]
        ys    = [0 .. h - 1]
        folder (_, _, '.') acc = acc
        folder (x, y, v  ) acc =
          if isDigit v && not (isDigit (Map.findWithDefault '.' (x - 1, y) chart))
          then let num    = takeWhile isDigit (drop x (ls !! y))
                   xs'    = [x - 1 .. x + length num]
                   ys'    = [y - 1 .. y + 1]
                   border = [Map.findWithDefault '.' (x', y') chart | x' <- xs', y' <- ys']
               in  if any (\a -> not (isDigit a || a == '.')) border
                   then acc + read num
                   else traceShow (num ++ " " ++ border) acc
          else acc
