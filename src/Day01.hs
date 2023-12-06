module Day01 (main) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  input <- lines <$> getContents
  putStrLn $ "Part 1: " ++ show (solve part1 input)
  putStrLn $ "Part 2: " ++ show (solve part2 input)

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
part2 s@(x:xs)
  | isDigit x = read [x] : part2 xs
  | "one"   `isPrefixOf` s = 1 : part2 xs
  | "two"   `isPrefixOf` s = 2 : part2 xs
  | "three" `isPrefixOf` s = 3 : part2 xs
  | "four"  `isPrefixOf` s = 4 : part2 xs
  | "five"  `isPrefixOf` s = 5 : part2 xs
  | "six"   `isPrefixOf` s = 6 : part2 xs
  | "seven" `isPrefixOf` s = 7 : part2 xs
  | "eight" `isPrefixOf` s = 8 : part2 xs
  | "nine"  `isPrefixOf` s = 9 : part2 xs
  | otherwise = part2 xs

