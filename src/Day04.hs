module Day04 (main) where

import System.Environment (getArgs)
import Data.List.Split (splitOneOf)
import Data.List (intersect, foldl')
import qualified Data.Map as Map

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  input <- getInput "inputs/day04.txt"
  let (p1, p2) = solve input
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

solve :: String -> (Int, Int)
solve input = (part1, part2)
  where parse s = case map (map read . words) (splitOneOf "|:" s) :: [[Int]] of
                    (_:a:b:_) -> length (a `intersect` b)
                    _         -> error "Invalid line"
        matches = zip [1..] (map parse (lines input))
        score n = if n > 0 then 2 ^ (n - 1) else 0
        part1   = sum (map (score . snd) matches)
        part2   = Map.foldr (+) 0 (foldl' (\m (i, n) ->
            let copy j = Map.insertWith (+) j (m Map.! i)
            in  foldr copy m [(i + 1) .. (min (i + n) (length input))]
          ) (Map.fromList [(i, 1) | i <- [1 .. length input]]) matches)

