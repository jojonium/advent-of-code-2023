module Day05 (main) where

import System.Environment (getArgs)
import Data.List.Split (splitOn, chunksOf)

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

type Range = (Int, Int)

main :: IO ()
main = do
  paras <- splitOn "\n\n" <$> getInput "inputs/day05.txt"
  let seeds  = map read $ drop 1 (words (head paras)) :: [Int]
      maps   = map (map (map read . words) . drop 1 . lines) (drop 1 paras)
      p1Seeds = map (\x -> (x, x)) seeds
      p2Seeds = map toRange (chunksOf 2 seeds)
  putStrLn $ "Part 1: " ++ show (solveR p1Seeds maps)
  putStrLn $ "Part 2: " ++ show (solveR p2Seeds maps)

toRange :: [Int] -> Range
toRange (a:b:_) = (a, a + b - 1)
toRange (a:_)   = (a, a)
toRange []      = error "Invalid range"

solveR :: [Range] -> [[[Int]]] -> Int
solveR seeds []     = minimum $ map fst seeds
solveR seeds (m:ms) = solveR (concatMap (convert m) seeds) ms
  where convert [] x = [x]
        convert ((drs:srs:l:_):ls) (r1, r2) =
          let sre       = srs + l
              leftover  = if r1 < srs then convert ls (r1, min r2 srs) else []
              rightover = if r2 > sre then convert ls (max r1 sre, r2) else []
              inter     = if r1 >= srs && r1 <= sre || r2 >= srs && r2 <= sre
                          then let i1 = max r1 srs; i2 = min r2 sre
                               in [(drs + (i1 - srs), drs + (i2 - srs))]
                          else []
          in  concat [leftover, inter, rightover]
        convert _ _ = error "Invalid line!"
