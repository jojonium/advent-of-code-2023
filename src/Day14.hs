module Day14 (main) where

import Data.List.Split (splitOn)
import Data.List (transpose)
import qualified Data.Map as Map

main :: IO ()
main = do
  input <- lines <$> getContents
  let height = length input 
      (chart, rounded, square, empty) = parse input
  print height
  putStrLn $ "Part 1: " ++ show (part1 chart height rounded)

type Coord = (Int, Int)
type Chart = Map.Map Coord Char

-- returns the chart and coords of all the Os, #s, and .s
parse :: [String] -> (Chart, [Coord], [Coord], [Coord])
parse ls = foldr folder (Map.empty, [], [], []) [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. length (head ls) - 1]
        ys = [0 .. length ls - 1]
        folder (x, y) (m, r, s, e) = (Map.insert (x, y) p m, r', s', e')
          where p  = ls !! y !! x
                r' = if p == 'O' then (x, y) : r else r
                s' = if p == '#' then (x, y) : s else s
                e' = if p == '.' then (x, y) : e else e

part1 :: Chart -> Int -> [Coord] -> Int
part1 chart h rounded = sum $ map ((h -) . snd) finalOs
  where finalOs = map (rollNorth chart) rounded

rollNorth :: Chart -> Coord -> Coord
rollNorth chart (x, y) = (x, fy + os)
  where above = takeWhile ((/='#') . (chart Map.!)) [(x, y') | y' <- reverse [0 .. y - 1]]
        os    = length $ filter ((=='O') . (chart Map.!)) above
        fy    = if null above then y else snd (last above)
