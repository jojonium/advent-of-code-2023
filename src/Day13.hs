module Day13 (main) where

import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = do
  patterns <- map lines . splitOn "\n\n" <$> getContents
  putStrLn $ "Part 1: " ++ show (sum (map solve patterns))

solve :: [String] -> Int
solve pattern = 100 * find 1 (transpose pattern) + find 1 pattern

find :: Int -> [String] -> Int
find n xs
  | n >= length (head xs) = 0 -- no vertical reflection
  | all (mirrored n) xs   = n
  | otherwise = find (n + 1) xs

mirrored :: Int -> String -> Bool
mirrored i line = let (a, b) = splitAt i line
                  in  all (uncurry (==)) (zip (reverse a) b)
