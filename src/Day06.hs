module Day06 (main) where

main :: IO ()
main = do
  input <- map (drop 1 . words) . lines <$> getContents
  let times     = map read $ head input
      distances = map read $ input !! 1
      p2Time    = read (concat (head input))
      p2Dist    = read (concat (input !! 1))
  putStrLn $ "Part 1: " ++ show (solve times distances)
  putStrLn $ "Part 2: " ++ show (solve [p2Time] [p2Dist])

solve :: [Double] -> [Double] -> Int
solve ts ds = product [waysToWin t d | (t, d) <- zip ts ds]
  
waysToWin :: Double -> Double -> Int
waysToWin t d = (upper - lower) + 1
  where lower = floor   $ (t - sqrt(t * t - 4 * d)) / 2 + 1
        upper = ceiling $ (t + sqrt(t * t - 4 * d)) / 2 - 1

