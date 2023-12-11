module Day11 (main) where

import Data.List (transpose, tails)

main :: IO ()
main = do
  input <- lines <$> getContents
  let emptyRows = (map snd . filter (all (=='.') . fst)) $ zip input [0..]
      emptyCols = (map snd . filter (all (=='.') . fst)) $ zip (transpose input) [0..]
      galaxies  = findGalaxies input
  putStrLn $ "Part 1: " ++ show (solve 2 galaxies emptyRows emptyCols)
  putStrLn $ "Part 2: " ++ show (solve 1000000 galaxies emptyRows emptyCols)

type Coord = (Int, Int)

findGalaxies :: [String] -> [Coord]
findGalaxies ls = foldr folder [] [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. length (head ls) - 1]
        ys = [0 .. length ls - 1]
        folder (x, y) gs = if p == '#' then (x, y) : gs else gs
          where p  = ls !! y !! x

expandManhattan :: Int -> [Int] -> [Int] -> Coord -> Coord -> Int
expandManhattan factor emptyRows emptyCols (x1, y1) (x2, y2) =
  (hx - lx + (crossedCols * (factor - 1))) + (hy - ly + (crossedRows * (factor - 1)))
  where lx = min x1 x2; ly = min y1 y2
        hx = max x1 x2; hy = max y1 y2
        crossedRows = length (filter (\a -> (a < hy) && (a > ly)) emptyRows)
        crossedCols = length (filter (\a -> (a < hx) && (a > lx)) emptyCols)

solve :: Int -> [Coord] -> [Int] -> [Int] -> Int
solve factor galaxies emptyRows emptyCols = 
  let pairs = [(a, b) | (a:bs) <- tails galaxies, b <- bs]
  in  sum $ map (uncurry (expandManhattan factor emptyRows emptyCols)) pairs

