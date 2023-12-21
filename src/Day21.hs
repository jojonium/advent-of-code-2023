{-# LANGUAGE TupleSections #-}
module Day21 (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Arrow ((***))

type Coord  = (Int, Int)
type Points = Set.Set Coord
type Memo = Map.Map Coord Int

main :: IO ()
main = do
  input <- lines <$> getContents
  let w = length (head input)
      h = length input
      (s, chart) = parse input w h
      initMemo = Map.fromList (map (, maxBound) (Set.toList chart))
      initMemo' = foldr Map.delete initMemo [(27, 80), (85, 45)] -- delete unreachable spots
      memo = evalState (fillMemo chart w h (Set.fromList [s])) (initMemo', 0)
  putStrLn $ "Part 1: " ++ show (solve memo 64)
  putStrLn $ "Part 2: " ++ show (solve2 26501365)
  -- let test = [let x = (65 + w * n) in (x, solveManually chart s w h x) | n <- [1, 2, 3]]
  -- putStrLn $ "Quadratic fit this: " ++ show (unwords (map show test))

parse :: [String] -> Int -> Int -> (Coord, Points)
parse ls w h = foldr folder ((0, 0), Set.empty) [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. w - 1]
        ys = [0 .. h - 1]
        folder (x, y) (s, plots) = case ls !! y !! x of
          '.' -> (s, Set.insert (x, y) plots)
          'S' -> ((x, y), Set.insert (x, y) plots)
          _   -> (s, plots)

walk :: Points -> Int -> Int -> Points -> Points
walk plots w h = Set.foldr (\o s -> foldr Set.insert s (walkOne o)) Set.empty
  where walkOne (x, y) = filter ((`Set.member` plots) . modulo) options
          where options  = [(x + 1, y) , (x - 1, y) , (x, y + 1) , (x, y - 1)]
                modulo = (`mod` w) *** (`mod` h)

fillMemo :: Points -> Int -> Int -> Points -> State (Memo, Int) Memo
fillMemo plots w h os = do
  (memo, step) <- get
  if all (< maxBound) (Map.elems memo) then do gets fst
  else do 
    let adjust coord = Map.insertWith min coord (step + 1)
        next  = Set.filter (\c -> Map.lookup c memo == Just maxBound) (walk plots w h os)
        memo' = foldr adjust memo next
    put (memo', step + 1)
    fillMemo plots w h next

solve :: Memo -> Int -> Int
solve memo n = length (filter helper (Map.elems memo))
  where helper x
          | even x    = x <= n && even n
          | otherwise = x <= n && odd n

solveManually :: Points -> Coord -> Int -> Int -> Int -> Int
solveManually plots s w h n = Set.size (iterate (walk plots w h) (Set.singleton s) !! n)

-- Part 2:
-- Run manually on [65 + w * n | n <- [1..]] and w = width of the input.
-- Once you get 3 results manually quadratic fit them and extrapolate to x = 26501365
solve2 :: Integer -> Integer
solve2 x =  ((15615 * x * x) + (27143 * x) - 106169) `div` 17161
