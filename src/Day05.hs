module Day05 (main) where

import System.Environment (getArgs)
import Data.List.Split (splitOn)

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  paras <- splitOn "\n\n" <$> getInput "inputs/day05.txt"
  let seeds  = map read $ drop 1 (words (head paras)) :: [Int]
      maps   = map (map (map read . words) . drop 1 . lines) (drop 1 paras)
  putStrLn $ "Part 1: " ++ show (solve seeds maps)


solve :: [Int] -> [[[Int]]] -> Int
solve seeds []     = minimum seeds
solve seeds (m:ms) = solve (map (convert m) seeds) ms
  where convert [] x = x
        convert ((dr:sr:l:_):ls) x
          | x >= sr && x <= sr + l = dr + (x - sr)
          | otherwise = convert ls x
        convert _ _ = error "Invalid line!"
