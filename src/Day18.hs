module Day18 (main) where

import Data.List (foldl')
import Debug.Trace (traceShow)
import qualified Data.Map as Map
import Control.Monad.State

type Coord = (Int, Int)
type Chart = Map.Map Coord Char
data Trench = Trench
  { tStart :: Coord
  , tEnd   :: Coord
  , tColor :: String
  , tVert  :: Bool
  } deriving (Eq, Ord, Show)

main :: IO ()
main = do
  trenches <- parse . lines <$> getContents
  putStrLn $ unlines (showTrenches trenches (3, 1))
  putStrLn $ "Part 1: " ++ show (evalState (solve (3, 1)) (toChart trenches))

parse :: [String] -> [Trench]
parse = fst . foldl' folder ([], (0, 0))
  where folder (ts, (x, y)) s = case head ws of
          "R" -> out (x + n, y) False
          "D" -> out (x, y + n) True
          "L" -> out (x - n, y) False
          "U" -> out (x, y - n) True
          _ -> error "Parse failed"
          where ws  = words s 
                n   = read (ws !! 1)
                col = take 7 (tail (ws !! 2))
                out end v = (ts ++ [Trench (x, y) end col v], end)

bounds :: [Trench] -> (Coord, Coord)
bounds = foldr folder ((0, 0), (0, 0))
  where folder (Trench {tEnd=(ex, ey)}) ((minx, miny), (maxx, maxy))=
          ((min minx ex, min miny ey), (max maxx ex, max maxy ey))

solve :: Coord -> State Chart Int
solve (x, y) = do
  chart <- get
  case Map.lookup (x, y) chart of
    Just _ -> return $ Map.size chart
    Nothing -> do
      let chart' = Map.insert (x, y) '#' chart
          neighbors = filter (`Map.notMember` chart') [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      put chart'
      maximum . (0:) <$> mapM solve neighbors

toChart :: [Trench] -> Chart
toChart = foldr helper Map.empty
  where helper (Trench {tStart=(sx, sy), tEnd=(_, ey), tVert=True}) m =
          foldr (`Map.insert` '#') m [(sx, y) | y <- [min sy ey .. max sy ey]]
        helper (Trench {tStart=(sx, sy), tEnd=(ex, _), tVert=False}) m =
          foldr (`Map.insert` '#') m [(x, sy) | x <- [min sx ex .. max sx ex]]

showTrenches :: [Trench] -> Coord -> [String]
showTrenches ts p = [[display (x, y) | x <- xs] | y <- ys]
  where chart = toChart ts
        ((minx, miny), (maxx, maxy)) = bounds ts
        xs = [minx .. maxx]
        ys = [miny .. maxy]
        display (a, b)
          | (a, b) == p = 'X'
          | (a, b) `Map.member` chart = '#'
          | otherwise = '.'

showChart :: Chart -> [String]
showChart chart = [[display (x, y) | x <- xs] | y <- ys]
  where coords = Map.keys chart
        xs = [minimum (map fst coords) .. maximum (map fst coords)]
        ys = [minimum (map snd coords) .. maximum (map snd coords)]
        display (a, b) = if (a, b) `Map.member` chart then '#' else '.'
