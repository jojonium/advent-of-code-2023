module Day13 (main) where

import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = do
  patterns <- map lines . splitOn "\n\n" <$> getContents
  let solve n p = 100 * mirrors n 1 (transpose p) + mirrors n 1 p 
  putStrLn $ "Part 1: " ++ show (sum (map (solve 0) patterns))
  putStrLn $ "Part 2: " ++ show (sum (map (solve 1) patterns))

-- target number of non-mirrored lines -> index to split on -> lines
mirrors :: Int -> Int -> [String] -> Int
mirrors t n xs
  | n >= length (head xs) = 0
  | length (filter (not . mirrored n) xs) == t = n
  | otherwise = mirrors t (n + 1) xs
  where mirrored i line = let (a, b) = splitAt i line
                          in  all (uncurry (==)) (zip (reverse a) b)

