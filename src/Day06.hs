module Day06 (main) where

import System.Environment (getArgs)

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  input <- lines <$> getInput "inputs/day06.txt"
  let parsed    = map (drop 1 . words) input
      -- times     = map read $ head parsed
      -- distances = map read $ parsed !! 1
      p2Time    = read (concat (head parsed)) :: Int
      p2Dist    = read (concat (parsed !! 1)) :: Int
  -- putStrLn $ "Part 1: " ++ show (solve times distances)
  putStrLn $ "Part 2: " ++ show (solve [p2Time] [p2Dist])

solve :: [Int] -> [Int] -> Int
solve ts ds = product [waysToWin t d | (t, d) <- zip ts ds]

waysToWin :: Int -> Int -> Int
waysToWin t d = length [h | h <- [1 .. (t - 1)], h * (t - h) > d]
