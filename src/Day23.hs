module Day23 (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Coord = (Int, Int)
type Chart = Map.Map Coord Char
type SearchState  = (Set.Set Coord, Coord)
type NeighborFunc = (SearchState -> [SearchState])

main :: IO ()
main = do
  input <- lines <$> getContents
  let w = length (head input)
      h = length input
      chart = parse input w h
      start = (1, 0)
      goal  = (w - 2, h - 1)
  putStrLn $ "Part 1: " ++ show (solve False chart start goal)
  putStrLn $ "Part 2: " ++ show (solve True chart start goal)

solve :: Bool -> Chart -> Coord -> Coord -> Int
solve p2 chart start goal = 
  let nf = neighbors p2 chart
  in  maximum (map length (allPaths chart nf (Set.singleton start, start) goal)) - 1

parse :: [String] -> Int -> Int -> Chart
parse ls w h = foldr folder Map.empty [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. w - 1]
        ys = [0 .. h - 1]
        folder (x, y) = Map.insert (x, y) (ls !! y !! x)

neighbors :: Bool -> Chart -> SearchState -> [SearchState]
neighbors p2 chart (seen, (x, y)) = 
  if p2 then out [north, south, east, west] 
  else case chart Map.! (x, y) of
    '.' -> out [north, south, east, west]
    '>' -> out [east]
    'v' -> out [south]
    '<' -> out [west]
    '^' -> out [north]
    _ -> error "Invalid char"
  where north = (x, y - 1)
        south = (x, y + 1)
        east  = (x + 1, y)
        west  = (x - 1, y)
        isntSeen  = (`Set.notMember` seen)
        validMove c
          | Just n <- Map.lookup c chart = n /= '#'
          | otherwise = False
        toSS c = (Set.insert c seen, c)
        out = map toSS . filter validMove . filter isntSeen

allPaths :: Chart -> NeighborFunc -> SearchState -> Coord -> [[SearchState]]
allPaths chart nf ss end
  | snd ss == end = [[ss]]
  | otherwise = [ss:path | nbr <- nf ss, path <- allPaths chart nf nbr end]

