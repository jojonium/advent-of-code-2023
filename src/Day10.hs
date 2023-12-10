module Day10 (main) where

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Debug.Trace (traceShow)
import Data.List (sortOn)

main :: IO ()
main = do
  input <- lines <$> getContents
  let (chart, s) = parse input
      memo = Map.fromList [(s, 0)]
      nbrs  = sNeighbors chart s
      memos = map (solve chart memo 0) nbrs
      joined = foldr1 (Map.unionWith min) memos
  -- putStrLn $ "Part 1: " ++ show (maximum (Map.elems solved))
  -- print $ (sortOn snd . Map.toList . head) memos
  -- print $ (sortOn snd . Map.toList . (!! 1)) memos
  putStrLn $ "Part 1: " ++ show (maximum (Map.elems joined))

type Coord = (Int, Int)
type Chart = Map.Map Coord Char
type Memo = Map.Map Coord Int
data Dir  = North | East | South | West deriving (Eq, Show)

parse :: [String] -> (Chart, Coord)
parse ls = foldr folder (Map.empty, (0, 0)) [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. length (head ls) - 1]
        ys = [0 .. length ls - 1]
        folder (x, y) (m, c) = (Map.insert (x, y) p m, c')
          where p  = ls !! y !! x
                c' = if p == 'S' then (x, y) else c

sNeighbors :: Chart -> Coord -> [(Coord, Dir)]
sNeighbors chart (x, y) = 
  let north = Map.findWithDefault '.' (x, y - 1) chart `elem` "|7F"
      south = Map.findWithDefault '.' (x, y + 1) chart `elem` "|LJ"
      west  = Map.findWithDefault '.' (x - 1, y) chart `elem` "-LF"
      east  = Map.findWithDefault '.' (x + 1, y) chart `elem` "-J7"
  in catMaybes
    [ if north then Just ((x, y - 1), North) else Nothing
    , if south then Just ((x, y + 1), South) else Nothing
    , if west  then Just ((x - 1, y), West ) else Nothing
    , if east  then Just ((x + 1, y), East ) else Nothing
    ]

next :: Chart -> (Coord, Dir) -> (Coord, Dir)
next chart ((x, y), facing) = case (chart Map.! (x, y), facing) of
  ('|', South) -> ((x, y + 1), South)
  ('|', North) -> ((x, y - 1), North)
  ('-', East ) -> ((x + 1, y), East)
  ('-', West ) -> ((x - 1, y), West)
  ('L', South) -> ((x + 1, y), East)
  ('L', West ) -> ((x, y - 1), North)
  ('J', South) -> ((x - 1, y), West)
  ('J', East ) -> ((x, y - 1), North)
  ('7', North) -> ((x - 1, y), West)
  ('7', East ) -> ((x, y + 1), South)
  ('F', North) -> ((x + 1, y), East)
  ('F', West ) -> ((x, y + 1), South)
  _ -> error $ "Invalid combination! " ++ show (x, y) ++ ", " ++ show facing

solve :: Chart -> Memo -> Int -> (Coord, Dir) -> Memo
solve chart memo s (cur, facing)
  | cur' `Map.member` memo = memo
  | otherwise = solve chart (Map.insert cur (s + 1) memo) (s + 1) (cur', facing')
  where (cur', facing') = next chart (cur, facing)
