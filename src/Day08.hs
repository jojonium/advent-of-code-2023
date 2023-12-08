module Day08 (main) where

import Control.Arrow ((&&&), first)
import qualified Data.Map as Map

main :: IO ()
main = do
  (ds, is) <- first cycle . (head &&& drop 2) . lines <$> getContents
  let chart = foldr parse Map.empty is
      srcs  = (map (take 3) . filter ((=='A') . (!!2))) is
  putStrLn $ "Part 1: " ++ show (part1 chart ds (=="ZZZ") 0 "AAA")
  putStrLn $ "Part 2: " ++ show (foldr1 lcm (map (part1 chart ds ((=='Z') . (!!2)) 0) srcs))

type Chart = Map.Map String (String, String)

parse :: String -> Chart -> Chart
parse s = Map.insert (take 3 s) (((take 3 . drop 7) &&& (take 3 . drop 12)) s)

-- chart, directions, end predicate, iteration counter, current node
part1 :: Chart -> String -> (String -> Bool) -> Integer -> String -> Integer
part1 chart (d:ds) p n cur
  | p cur = n
  | otherwise = part1 chart ds p (n + 1) next
  where next = (if d == 'L' then fst else snd) (chart Map.! cur)
part1 _ [] _ _ _ = error "Ran out of instructions"

