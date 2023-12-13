module Day13 (main) where

import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = do
  patterns <- map lines . splitOn "\n\n" <$> getContents
  let p1 = map (\p -> 100 * solve 0 1 (transpose p) + solve 0 1 p) patterns
      p2 = map (\p -> 100 * solve 1 1 (transpose p) + solve 1 1 p) patterns
  putStrLn $ "Part 1: " ++ show (sum p1)
  putStrLn $ "Part 2: " ++ show (sum p2)

mirrored :: Int -> String -> Bool
mirrored i line = let (a, b) = splitAt i line
                  in  all (uncurry (==)) (zip (reverse a) b)

-- target number of non-mirrored lines -> index to split on -> lines
solve :: Int -> Int -> [String] -> Int
solve t n xs
  | n >= length (head xs) = 0
  | length (filter (not . mirrored n) xs) == t = n
  | otherwise = solve t (n + 1) xs

