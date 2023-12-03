module Day03 (main) where

import System.Environment (getArgs)
import Data.Char (isDigit)
import qualified Data.Map as Map

type Coord = (Int, Int)
type Chart = Map.Map Coord Char

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  input <- lines <$> getInput "inputs/day03.txt"
  let (part1, part2) = solve input
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2

parse :: [String] -> Int -> Int -> Chart
parse ls w h = foldr folder Map.empty [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. w - 1]
        ys = [0 .. h - 1]
        folder (x, y) = Map.insert (x, y) p
          where p = ls !! y !! x

solve :: [String] -> (Int, Int)
solve ls = (part1, part2)
  where w     = length (head ls)
        h     = length ls
        chart = parse ls w h
        xs    = [0 .. w - 1]
        ys    = [0 .. h - 1]
        parts = foldr folder [] [(x, y, chart Map.! (x, y)) | x <- xs, y <- ys]
        folder (_, _, '.') acc = acc
        folder (x, y, v  ) acc =
          if isDigit v && not (isDigit (Map.findWithDefault '.' (x - 1, y) chart))
          then let num    = takeWhile isDigit (drop x (ls !! y))
                   xs'    = [x - 1 .. x + length num]
                   ys'    = [y - 1 .. y + 1]
                   border = [(x', y', Map.findWithDefault '.' (x', y') chart) | x' <- xs', y' <- ys']
                   symbs  = filter (\(_, _, a) -> not (isDigit a || a == '.')) border
               in  (num, symbs) : acc
          else acc
        part1 = sum (map (read . fst) (filter (not . null . snd) parts))
        (counts, nums)  = foldr p2f (Map.empty, Map.empty) parts
        p2f (n, (x, y, '*'):_) (cs, ns) = (Map.insertWith (+) (x, y) 1 cs, Map.insertWith (*) (x, y) (read n) ns)
        p2f _                  (cs, ns) = (cs, ns)
        gears = filter ((==2) . snd) (Map.toList counts)
        ratio = map ((nums Map.!) . fst) gears
        part2 = sum ratio


