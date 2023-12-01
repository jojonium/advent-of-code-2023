module Day01 (main) where

import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.List (isPrefixOf)

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  input <- getInput "inputs/day01.txt"
  putStrLn $ "Part 1: " ++ show (solve part1 (lines input))
  putStrLn $ "Part 2: " ++ show (solve part2 (lines input))

solve :: (String -> [Int]) -> [String] -> Int
solve f ls =  sum (map calibVal ls)
  where calibVal s = let nums = f s
                     in  head nums * 10 + last nums

part1 :: String -> [Int]
part1 [] = []
part1 (x:xs)
  | isDigit x = read [x] : part1 xs
  | otherwise = part1 xs

part2 :: String -> [Int]
part2 [] = []
part2 s
  | isDigit (head s) = read [head s] : part2 remain
  | "one"   `isPrefixOf` s = 1 : part2 remain
  | "two"   `isPrefixOf` s = 2 : part2 remain
  | "three" `isPrefixOf` s = 3 : part2 remain
  | "four"  `isPrefixOf` s = 4 : part2 remain
  | "five"  `isPrefixOf` s = 5 : part2 remain
  | "six"   `isPrefixOf` s = 6 : part2 remain
  | "seven" `isPrefixOf` s = 7 : part2 remain
  | "eight" `isPrefixOf` s = 8 : part2 remain
  | "nine"  `isPrefixOf` s = 9 : part2 remain
  | otherwise = part2 remain
  where remain = tail s

