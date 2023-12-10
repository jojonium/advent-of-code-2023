module Day10 (main) where

import qualified Data.Map as Map
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  input <- lines <$> getContents
  let (chart, s) = parse input
      memo = Map.fromList [(s, 0)]
      solved = solve chart memo s
  putStrLn $ "Part 1: " ++ show (maximum (Map.elems solved))
  

type Coord = (Int, Int)
type Chart = Map.Map Coord Char
type Memo = Map.Map Coord Int

parse :: [String] -> (Chart, Coord)
parse ls = foldr folder (Map.empty, (0, 0)) [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. length (head ls) - 1]
        ys = [0 .. length ls - 1]
        folder (x, y) (m, c) = (Map.insert (x, y) p m, c')
          where p  = ls !! y !! x
                c' = if p == 'S' then (x, y) else c

neighbors :: Chart -> Coord -> [Coord]
neighbors chart (x, y) = case Map.findWithDefault '.' (x, y) chart of
  '.' -> []
  '|' -> [(x, y + 1), (x, y - 1)]
  '-' -> [(x + 1, y), (x - 1, y)]
  'L' -> [(x, y - 1), (x + 1, y)]
  'J' -> [(x, y - 1), (x - 1, y)]
  '7' -> [(x, y + 1), (x - 1, y)]
  'F' -> [(x, y + 1), (x + 1, y)]
  'S' -> sNeighbors
  _   -> error $ "Invalid char: " ++ [chart Map.! (x, y)]
  where north = Map.findWithDefault '.' (x, y - 1) chart
        south = Map.findWithDefault '.' (x, y + 1) chart
        west  = Map.findWithDefault '.' (x - 1, y) chart
        east  = Map.findWithDefault '.' (x + 1, y) chart
        sNeighbors = catMaybes
          [ if north `elem` "|7F" then Just (x, y - 1) else Nothing
          , if south `elem` "|LJ" then Just (x, y + 1) else Nothing
          , if west  `elem` "-LF" then Just (x - 1, y) else Nothing
          , if east  `elem` "-J7" then Just (x + 1, y) else Nothing
          ]

solve :: Chart -> Memo -> Coord -> Memo
solve chart memo cur = foldr (Map.unionWith min . solve chart memo') memo' nbrs'
  where nbrs  = neighbors chart cur
        nbrs' = filter (`Map.notMember` memo) nbrs
        prevL = memo Map.! cur
        memo' = foldr (\n m -> Map.insertWith min n (prevL + 1) m) memo nbrs'
