module Day08 (main) where

import Control.Arrow ((&&&), (***), first)
import qualified Data.Map as Map

main :: IO ()
main = do
  (ds, is) <- first cycle . (head &&& drop 2) . lines <$> getContents
  let chart = foldr parse Map.empty is
      srcs  = (map (take 3) . filter ((=='A') . (!!2))) is
  putStrLn $ "Part 1: " ++ show (part1 chart ds (=="ZZZ") 0 "AAA")
  putStrLn $ "Part 2: " ++ show (part2 chart ds srcs ((=='Z') . (!!2)))

type Chart = Map.Map String (String, String)

parse :: String -> Chart -> Chart
parse s = Map.insert (take 3 s) ((take 3 *** take 3) ((drop 7 &&& drop 12) s))

part1 :: Chart -> String -> (String -> Bool) -> Integer -> String -> Integer
part1 chart (d:ds) p n cur
  | p cur = n
  | otherwise = part1 chart ds p (n + 1) next
  where next = (if d == 'L' then fst else snd) (chart Map.! cur)
part1 _ [] _ _ _ = error "Ran out of instructions"

part2 :: Chart -> String -> [String] -> (String -> Bool) -> Integer
part2 chart dirs sources p = foldr1 lcm x
  where x = map (part1 chart dirs p 0) sources
