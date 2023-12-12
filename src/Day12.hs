module Day12 (main) where

import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Function (fix)
import Control.Arrow ((***))
import qualified Data.Map as Map

main :: IO ()
main = do
  input <- map words . lines <$> getContents
  let rows = map (\s -> (head s, map read (splitOn "," (s !! 1)))) input
      unfolded = map ((intercalate "?" . replicate 5) *** (concat . replicate 5)) rows
  putStrLn $ "Part 2: " ++ show (sum $ map (fst . uncurry (fix (memoize . solve) Map.empty)) rows)
  putStrLn $ "Part 2: " ++ show (sum $ map (fst . uncurry (fix (memoize . solve) Map.empty)) unfolded)

type Memo = Map.Map (String, [Int]) Integer
type SolveFunc = (Memo -> String -> [Int] -> (Integer, Memo))

memoize :: SolveFunc -> Memo -> String -> [Int] -> (Integer, Memo)
memoize f m ss xs = case Map.lookup (ss, xs) m of
  Just n  -> (n, m)
  Nothing -> let (n, m') = f m ss xs
             in (n, Map.insert (ss, xs) n m')

solve :: SolveFunc -> Memo -> String -> [Int] -> (Integer, Memo)
solve _ m ss []
  | '#' `elem` ss = (0, m)  -- ran out of numbers with '#'s remaining
  | otherwise     = (1, m)  -- reached end of numbers successfully
solve _ m [] _    = (0, m)  -- ran out of space in string
solve f m ss@(s:st) xs
  | length ss < (sum xs + length xs - 1) = (0, m)  -- ran out of space in string
  | s == '.' = f m st xs
  | s == '#' = fit f m ss xs
  | s == '?' = let (ifBroken,  m1) = fit f m ss xs
                   (ifWorking, m2) = f m1 st xs
               in  (ifBroken + ifWorking, m2)
  | otherwise = error $ "This shouldn't happen " ++ ss ++ ", " ++ show xs

fit :: SolveFunc -> Memo -> String -> [Int] -> (Integer, Memo)
fit f m ss (x:xt)
  | all (`elem` "#?") removed = fitWorking f m remain xt
  | otherwise = (0, m)
  where (removed, remain) = splitAt x ss
fit _ m _ [] = (0, m)

fitWorking :: SolveFunc -> Memo -> String -> [Int] -> (Integer, Memo)
fitWorking _ m [] [] = (1, m)
fitWorking _ m [] _  = (0, m)
fitWorking f m (s:st) xs
  | s `elem` ".?" = f m st xs
  | otherwise     = (0, m)
