module Day14 (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Coord = (Int, Int)
type Pnts  = Set.Set Coord
data Dir   = North | South | East | West

main :: IO ()
main = do
  input <- lines <$> getContents
  let height = length input 
      width  = length (head input)
      (rounded, square) = parse input
      p1 = rollAll North square width height rounded
  putStrLn $ "Part 1: " ++ show (sum $ map ((height -) . snd) (Set.elems p1))
  let (a, b, r) = findRepeat (doCycle square width height) rounded
      toRun = (1000000000 - b) `mod` (b - a)
      p2    = iterate (doCycle square width height) r !! toRun
  putStrLn $ "Part 2: " ++ show (sum $ map ((height -) . snd) (Set.elems p2))

-- Returns the coords of all square rocks and all rounded rocks
parse :: [String] -> (Pnts, Pnts)
parse ls = foldr folder (Set.empty, Set.empty) [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. length (head ls) - 1]
        ys = [0 .. length ls - 1]
        folder (x, y) (r, s) = (r', s')
          where p  = ls !! y !! x
                r' = if p == 'O' then Set.insert (x, y) r else r
                s' = if p == '#' then Set.insert (x, y) s else s

rollAll :: Dir -> Pnts -> Int -> Int -> Pnts -> Pnts
rollAll d square w h rounded = Set.map roll rounded 
  where roll (x, y) = case d of
          North -> let fy = if null above then y else snd (last above)
                   in  (x, fy + os above)
          South -> let fy = if null below then y else snd (last below)
                   in  (x, fy - os below)
          East  -> let fx = if null right then x else fst (last right)
                   in  (fx - os right, y)
          West  -> let fx = if null left  then x else fst (last left)
                   in  (fx + os left, y)
          where rollTo = takeWhile (`Set.notMember` square)
                above  = rollTo [(x, y') | y' <- [y - 1, y - 2 .. 0]]
                left   = rollTo [(x', y) | x' <- [x - 1, x - 2 .. 0]]
                below  = rollTo [(x, y') | y' <- [y + 1 .. h - 1]]
                right  = rollTo [(x', y) | x' <- [x + 1 .. w - 1]]
                os cs  = length $ filter (`Set.member` rounded) cs

doCycle :: Pnts -> Int -> Int -> Pnts -> Pnts
doCycle square w h rounded = rollAll East square w h south
  where north = rollAll North square w h rounded
        west  = rollAll West square w h north
        south = rollAll South square w h west

-- Run until we find a repeated state, then return the indexes of the first and
-- second time we've seen it, along with the state itself
findRepeat :: (Pnts -> Pnts) -> Pnts -> (Int, Int, Pnts)
findRepeat f initial = helper initial Map.empty 0
  where helper cur prev count
          | Just n <- Map.lookup cur prev = (n, count, cur)
          | otherwise = helper (f cur) (Map.insert cur count prev) (count + 1)

