module Day21 (main) where

import qualified Data.Set as Set

type Coord  = (Int, Int)
type Points = Set.Set Coord

main :: IO ()
main = do
  input <- lines <$> getContents
  let w = length (head input)
      h = length input
      (s, chart) = parse input w h
  putStrLn $ "Part 1: " ++ show (Set.size (iterate (walk chart) (Set.fromList [s]) !! 64))

parse :: [String] -> Int -> Int -> (Coord, Points)
parse ls w h = foldr folder ((0, 0), Set.empty) [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. w - 1]
        ys = [0 .. h - 1]
        folder (x, y) (s, acc) = case ls !! y !! x of
          '.' -> (s, Set.insert (x, y) acc)
          'S' -> ((x, y), Set.insert (x, y) acc)
          _   -> (s, acc)

walk :: Points -> Points -> Points
walk plots = Set.foldr (\o s -> foldr Set.insert s (walkOne o)) Set.empty
  where walkOne (x, y) = filter (`Set.member` plots) options
          where options = [(x + 1, y) , (x - 1, y) , (x, y + 1) , (x, y - 1)]
