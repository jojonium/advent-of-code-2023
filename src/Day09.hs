module Day09 (main) where

main :: IO ()
main = do
  seqs <- map (map read . words) . lines <$> getContents
  putStrLn $ "Part 1: " ++ show (sum (map (last . solve) seqs))
  putStrLn $ "Part 2: " ++ show (sum (map (head . solve) seqs))

solve :: [Int] -> [Int]
solve xs
  | all (==0) next = head xs : head xs : xs
  | otherwise      = newHead : xs ++ [newEnd]
  where next    = zipWith subtract xs (tail xs)
        newEnd  = last xs + last (solve next)
        newHead = head xs - head (solve next)

