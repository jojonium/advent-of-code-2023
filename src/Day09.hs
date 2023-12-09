module Day09 (main) where

import Control.Arrow ((&&&), first)
import qualified Data.Map as Map

main :: IO ()
main = do
  seqs <- map (map read . words) . lines <$> getContents
  putStrLn $ "Part 1: " ++ show (sum (map (last . solve) seqs))

solve :: [Int] -> [Int]
solve xs
  | all (==0) next = head xs : xs
  | otherwise      = xs ++ [last xs + last (solve next)]
  where next = zipWith subtract xs (tail xs)

