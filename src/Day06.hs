module Day06 (main) where

main :: IO ()
main = do
  [ts, ds] <- map (drop 1 . words) . lines <$> getContents
  putStrLn $ "Part 1: " ++ show (solve (map read ts) (map read ds))
  putStrLn $ "Part 2: " ++ show (solve [read (concat ts)] [read (concat ds)])

solve :: [Double] -> [Double] -> Int
solve ts ds = product 
  [ 1 + ceiling ((t + sqrt (t * t - 4 * d)) / 2 - 1) -
        floor   ((t - sqrt (t * t - 4 * d)) / 2 + 1)
  | (t, d) <- zip ts ds]
  
