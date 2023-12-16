module Day16 (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

type Coord = (Int, Int)
data Dir   = North | South | East | West deriving (Show, Eq, Ord)
type Chart = Map.Map Coord Char
type Seen  = Map.Map Coord Dir

main :: IO ()
main = do
  chart <- parse . lines <$> getContents
  let p1 = evalState (solve chart ((0, 0), East)) Map.empty
  putStrLn $ "Part 1: " ++ show p1

parse :: [String] -> Chart
parse ls = foldr folder Map.empty [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. length (head ls) - 1]
        ys = [0 .. length ls - 1]
        folder (x, y) = Map.insert (x, y) (ls !! y !! x)

solve :: Chart -> (Coord, Dir) -> State Seen Int
solve chart (coord, dir) = do
  seen <- get
  if Map.lookup coord seen == Just dir
  then return (Map.size seen)
  else do
    let seen' = Map.insert coord dir seen
        nexts = case Map.lookup coord chart of
          Just c  -> next coord dir c
          Nothing -> []
    put seen'
    case filter ((`Map.member` chart) . fst) nexts of
      []  -> do return (Map.size seen')
      [a] -> do solve chart a
      (a:b:_) -> do
        _ <- solve chart a
        solve chart b

to :: Dir -> Coord -> (Coord, Dir)
to North (x, y) = ((x, y - 1), North)
to South (x, y) = ((x, y + 1), South)
to East  (x, y) = ((x + 1, y), East)
to West  (x, y) = ((x - 1, y), West)

next :: Coord -> Dir -> Char -> [(Coord, Dir)]
next coord dir   '.'  = [to dir coord]
next coord North '/'  = [to East coord]
next coord South '/'  = [to West coord]
next coord East  '/'  = [to North coord]
next coord West  '/'  = [to South coord]
next coord North '\\' = [to West coord]
next coord South '\\' = [to East coord]
next coord East  '\\' = [to South coord]
next coord West  '\\' = [to North coord]
next coord North '|'  = [to North coord]
next coord South '|'  = [to South coord]
next coord East  '|'  = [to North coord, to South coord]
next coord West  '|'  = [to North coord, to South coord]
next coord North '-'  = [to West coord, to East coord]
next coord South '-'  = [to West coord, to East coord]
next coord East  '-'  = [to East coord]
next coord West  '-'  = [to West coord]
next _ _  _ = error "I don't know what to do here"


showChart :: Chart -> Coord -> String
showChart chart coord = unlines [[display (x, y) | x <- xs] | y <- ys]
  where xs = [0 .. maximum (map (fst . fst) (Map.toList chart))]
        ys = [0 .. maximum (map (snd . fst) (Map.toList chart))]
        display (a, b) = if (a, b) == coord then 'X' else chart Map.! (a, b)

showSeen :: Set.Set (Coord, Dir) -> String
showSeen seen = unlines [[if inSeen (x, y) then '#' else '.'| x <- xs] | y <- ys]
  where xs = [0 .. maximum (map (fst . fst) (Set.toList seen))]
        ys = [0 .. maximum (map (snd . fst) (Set.toList seen))]
        inSeen c = (c, North) `Set.member` seen ||
                   (c, South) `Set.member` seen ||
                   (c, East)  `Set.member` seen ||
                   (c, West)  `Set.member` seen
