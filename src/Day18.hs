module Day18 (main) where

data Dir = U | R | D | L deriving (Eq, Show)

main :: IO ()
main = do
  input <- lines <$> getContents
  putStrLn $ "Part 1: " ++ show (solve (map parse1 input))
  putStrLn $ "Part 1: " ++ show (solve (map parse2 input))

parse1 :: String -> (Integer, Dir)
parse1 s = (read (ws !! 1), dir)
  where ws  = words s 
        dir = case head ws of 
          "R" -> R; "D" -> D; "L" -> L; "U" -> U
          _ -> error "Parse failed"

parse2 :: String -> (Integer, Dir)
parse2 s = (n, dir)
  where ws  = words s 
        n   = read ("0x" ++ take 5 (drop 2 (ws !! 2)))
        dir = case (ws !! 2) !! 7 of 
          '0' -> R; '1' -> D; '2' -> L; '3' -> U
          _ -> error "Parse failed"

solve :: [(Integer, Dir)] -> Integer
solve ls = inside + perimeter
  where (area, perimeter, _) = foldr folder (0, 0, (0, 0)) ls
        folder (n, dir) (a, p, (x, y))  = case dir of
          R -> next (x + n, y)
          D -> next (x, y + n)
          L -> next (x - n, y)
          U -> next (x, y - n)
          where next (x', y') = (a + (x * y' - y * x'), p + n, (x', y'))
        area'  = abs area `div` 2
        inside = area' - (perimeter `div` 2) + 1
