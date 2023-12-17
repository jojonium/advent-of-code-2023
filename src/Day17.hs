module Day17 (main) where

import qualified Data.Map as Map
import Algorithm.Search (aStar)

type Coord = (Int, Int)
data Dir   = North | South | East | West deriving (Show, Eq, Ord)
type Chart = Map.Map Coord Int
data Pos = Pos
  { pCoord :: Coord
  , pDir   :: Dir
  , pStr8  :: Int
  } deriving (Eq, Ord, Show)

main :: IO ()
main = do
  input <- lines <$> getContents
  let w = length (head input)
      h = length input
      chart = parse input w h
  case solve chart (neighbors w h) (w - 1, h - 1) (Pos (0, 0) East 0) of
    Just (p1, _) -> do putStrLn $ "Part 1: " ++ show p1
    Nothing -> do putStrLn "Nothing"
  case solve chart (neighbors2 w h) (w - 1, h - 1) (Pos (0, 0) East 0) of
    Just (p2, _) -> do putStrLn $ "Part 2: " ++ show p2
    Nothing -> do putStrLn "Nothing"

parse :: [String] -> Int -> Int -> Chart
parse ls w h = foldr folder Map.empty [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. w - 1]
        ys = [0 .. h - 1]
        folder (x, y) = Map.insert (x, y) (read [ls !! y !! x])

solve :: Chart -> (Pos -> [Pos]) -> Coord -> Pos -> Maybe (Int, [Pos])
solve chart neighborFunc goal =
  aStar neighborFunc (cost chart) (manhattan goal . pCoord) ((== goal) . pCoord)

neighbors :: Int -> Int -> Pos -> [Pos]
neighbors w h  pos = filter (valid . pCoord) possible
  where possible = left pos : right pos : [straight pos | pStr8 pos < 2]
        valid (x, y) = x >= 0 && x < w && y >= 0 && y < h

neighbors2 :: Int -> Int -> Pos -> [Pos]
neighbors2 w h pos@(Pos {pStr8=n}) = filter (valid . pCoord) possible
  where possible = (if n >= 3 then [left pos, right pos] else []) ++ [straight pos | n < 9]
        valid (x, y) = x >= 0 && x < w && y >= 0 && y < h

straight :: Pos -> Pos
straight (Pos (x, y) North n) = Pos (x, y - 1) North (n + 1)
straight (Pos (x, y) East  n) = Pos (x + 1, y) East  (n + 1)
straight (Pos (x, y) South n) = Pos (x, y + 1) South (n + 1)
straight (Pos (x, y) West  n) = Pos (x - 1, y) West  (n + 1)

left :: Pos -> Pos
left (Pos (x, y) North _) = Pos (x - 1, y) West  0
left (Pos (x, y) East  _) = Pos (x, y - 1) North 0
left (Pos (x, y) South _) = Pos (x + 1, y) East  0
left (Pos (x, y) West  _) = Pos (x, y + 1) South 0

right :: Pos -> Pos
right (Pos (x, y) North _) = Pos (x + 1, y) East  0
right (Pos (x, y) East  _) = Pos (x, y + 1) South 0
right (Pos (x, y) South _) = Pos (x - 1, y) West  0
right (Pos (x, y) West  _) = Pos (x, y - 1) North 0

cost :: Chart -> Pos -> Pos -> Int
cost chart _ (Pos {pCoord=(x, y)}) = chart Map.! (x, y)

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
