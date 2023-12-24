module Day23 (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Coord = (Int, Int)
type Chart = Map.Map Coord Char
type Maze  = Map.Map Coord [Coord]
type SearchState  = (Set.Set Coord, Coord)

main :: IO ()
main = do
  input <- lines <$> getContents
  let w = length (head input)
      h = length input
      chart = parse input w h
      start = (1, 0)
      goal  = (w - 2, h - 1)
      p1Maze  = toMaze False chart
      p2Maze  = toMaze True  chart
  putStrLn $ "Part 1: " ++ show (maximum (pathHelper p1Maze (Set.singleton start, start) goal))
  putStrLn $ "Part 2: " ++ show (increasing 0 (pathHelper p2Maze (Set.singleton start, start) goal))

increasing :: Int -> [Int] -> [Int]
increasing _ [] = []
increasing prev (x:xs)
  | x > prev = x : increasing x xs
  | otherwise = increasing prev xs

parse :: [String] -> Int -> Int -> Chart
parse ls w h = foldr folder Map.empty [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. w - 1]
        ys = [0 .. h - 1]
        folder (x, y) m = let p = ls !! y !! x
                          in if p == '#' then m else Map.insert (x, y) p m

toMaze :: Bool -> Chart -> Maze
toMaze p2 chart = Map.mapWithKey (neighbors p2 chart) chart

neighbors :: Bool -> Chart -> Coord -> Char -> [Coord]
neighbors p2 chart (x, y) char = filter (`Map.member` chart) potential
  where
    potential = if p2 then [north, south, east, west]
      else case char of
        '.' -> [north, south, east, west]
        '>' -> [east]
        'v' -> [south]
        '<' -> [west]
        '^' -> [north]
        _ -> error $ "Invalid char " ++ [char]
    north = (x, y - 1)
    south = (x, y + 1)
    east  = (x + 1, y)
    west  = (x - 1, y)

pathHelper :: Maze -> SearchState -> Coord -> [Int]
pathHelper maze (seen, coord) end
  | coord == end = [0]
  | otherwise = [1 + nDist | node <- edges, nDist <- pathHelper maze (toSS node) end]
  where edges  = filter (`Set.notMember` seen) (maze Map.! coord)
        toSS c = (Set.insert c seen, c)

