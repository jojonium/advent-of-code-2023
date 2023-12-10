module Day10 (main) where

import qualified Data.Map as Map

main :: IO ()
main = do
  (chart, s) <- parse . lines <$> getContents
  let (nbrs, sChar) = sNeighbors chart s
      memos         = map (solve chart (Map.fromList [(s, 0)]) 0) nbrs
      joined        = foldr1 (Map.unionWith min) memos
      chart'        = Map.insert s sChar chart
  putStrLn $ "Part 1: " ++ show (maximum (Map.elems joined))
  putStrLn $ "Part 2: " ++ show (length (part2 chart' joined))

type Coord = (Int, Int)
type Chart = Map.Map Coord Char
type Memo = Map.Map Coord Int
data Dir  = North | East | South | West deriving (Eq, Show)

-- Parse input into a Map of (X, Y) -> Char
parse :: [String] -> (Chart, Coord)
parse ls = foldr folder (Map.empty, (0, 0)) [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. length (head ls) - 1]
        ys = [0 .. length ls - 1]
        folder (x, y) (m, c) = (Map.insert (x, y) p m, c')
          where p  = ls !! y !! x
                c' = if p == 'S' then (x, y) else c

-- Figure out what char S must be covering, return it and S's neighbors
sNeighbors :: Chart -> Coord -> ([(Coord, Dir)], Char)
sNeighbors chart (x, y)
  | north && south = ([northN, southN], '|')
  | east  && west  = ([eastN, westN], '-')
  | north && east  = ([northN, eastN], 'L')
  | north && west  = ([northN, westN], 'J')
  | south && east  = ([southN, eastN], 'F')
  | south && west  = ([southN, westN], '7')
  | otherwise = error "This shouldn't happen"
  where north = Map.findWithDefault '.' (x, y - 1) chart `elem` "|7F"
        south = Map.findWithDefault '.' (x, y + 1) chart `elem` "|LJ"
        west  = Map.findWithDefault '.' (x - 1, y) chart `elem` "-LF"
        east  = Map.findWithDefault '.' (x + 1, y) chart `elem` "-J7"
        northN = ((x, y - 1), North)
        southN = ((x, y + 1), South)
        westN  = ((x - 1, y), West )
        eastN  = ((x + 1, y), East )

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

-- Traverse the pipe in one direction.
solve :: Chart -> Memo -> Int -> (Coord, Dir) -> Memo
solve chart memo s (cur, facing)
  | cur' `Map.member` memo = memo
  | otherwise = solve chart (Map.insert cur (s + 1) memo) (s + 1) (cur', facing')
  where (cur', facing') = next chart (cur, facing)

-- Draw a ray from the current point westward, counting the number of times it
-- intersects the pipe. If the number of intersections is odd we are enclosed.
part2 :: Chart -> Memo -> [Coord]
part2 chart solved = filter insideLoop (Map.keys (chart Map.\\ solved))
  where insideLoop (x, y) =
          let toWest = [(x', y) | x' <- [0 .. x - 1]]
              hits   = filter (`Map.member` solved) toWest
              hits'  = filter (/='-') (map (chart Map.!) hits)
          in  odd (intersections hits')

-- Vertical walls and corners that form S or Z shapes count as intersections,
-- corners that form U shapes don't.
intersections :: String -> Int
intersections ('|':xs)     = 1 + intersections xs
intersections ('L':'7':xs) = 1 + intersections xs
intersections ('F':'J':xs) = 1 + intersections xs
intersections ('L':'J':xs) = intersections xs
intersections ('F':'7':xs) = intersections xs
intersections _ = 0
