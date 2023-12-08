module Day07 (main) where

import Control.Arrow ((&&&), (***))
import qualified Data.Map as Map

main :: IO ()
main = do
  (ds, is) <- (head &&& drop 2) . lines <$> getContents
  let chart = foldr parse Map.empty is
      dirs  = cycle ds
  putStrLn $ "Part 1: " ++ show (travel chart dirs "AAA" 0)

type Chart = Map.Map String (String, String)

parse :: String -> Chart -> Chart
parse s = Map.insert (take 3 s) ((take 3 *** take 3) ((drop 7 &&& drop 12) s))

travel :: Chart -> String -> String -> Int -> Int
travel _ _ "ZZZ" n = n
travel chart (d:ds) cur n = travel chart ds (side (chart Map.! cur)) (n + 1)
  where side = if d == 'L' then fst else snd
travel _ [] _ _ = error "Ran out of instructions"
