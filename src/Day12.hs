module Day12 (main) where

import Data.List.Split (splitOn)
import Data.List (intercalate)
import Control.Arrow ((***))

main :: IO ()
main = do
  input <- map words . lines <$> getContents
  let rows = map (\s -> (head s, map read (splitOn "," (s !! 1)))) input
  -- putStrLn $ "Part 1: " ++ show (sum (map (uncurry solve) rows))
  let unfolded = map ((intercalate "?" . replicate 5) *** (concat . replicate 5)) rows
  -- putStrLn $ "Part 2: " ++ show (sum (map (uncurry solve) unfolded))
  print (sum (map (uncurry solve) unfolded))

solve :: String -> [Int] -> Int
solve ss [] 
  | '#' `elem` ss = 0 -- ran out of numbers with '#'s remaining
  | otherwise = 1     -- reached end of numbers successfully
solve [] (_:_) = 0    -- ran out of space in string
solve ss@(s:st) xs@(x:xt)
  | length ss < x = 0      -- ran out of space in string
  | s == '.' = solve st xs -- skip this working spring
  | s == '#' = if fits x ss                    -- try fitting current number
               then solve (drop (x + 1) ss) xt -- fits here, continue
               else 0                          -- doesn't fit here
  | s == '?' = solve ('#':st) xs + solve st xs -- try both options for '?'
  | otherwise = error "This shouldn't happen"

fits :: Int -> String -> Bool
fits n ss = all (`elem` "#?") (take n ss) && endFits
  where endFits = length ss == n || (`elem` ".?") (ss !! n)
