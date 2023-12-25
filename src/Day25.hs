module Day25 (main) where

import Data.List (delete)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Graph = Map.Map String [String]
type Edge  = (String, String)

main :: IO ()
main = do
  graph <- parse . lines <$> getContents

  -- pipe thie output into graphviz (layout=neato) to find the 3 edges to remove
  --let edges = toEdges graph
  --putStrLn "graph G {"
  --putStrLn "  layout=neato"
  --putStr (graphviz (removeDups Set.empty edges))
  --putStrLn "}"

  -- Edges to remove: hcf -> lhn, ldl -> fpg, nxk -> dfk
  let g1 = removeEdge graph ("hcf", "lhn")
      g2 = removeEdge g1 ("ldl", "fpg")
      g3 = removeEdge g2 ("nxk", "dfk")
  print (connectedSize g3 "hcf")
  print (connectedSize g3 "lhn")
  putStrLn $ "Part 1: " ++ show (connectedSize g3 "hcf" * connectedSize g3 "lhn")

parse :: [String] -> Graph
parse = foldr helper Map.empty
  where helper str m =
          let (a, b) = break (==':') str
              right  = words (drop 2 b)
              m'     = Map.insertWith (++) a right m
          in  foldr (\l acc -> Map.insertWith (++) l [a] acc) m' right

toEdges :: Graph -> [Edge]
toEdges = concatMap (\(a, bs) -> [(a, b) | b <- bs]) . Map.toList

removeDups :: Set.Set Edge -> [Edge] -> [Edge]
removeDups prev [] = Set.toList prev
removeDups prev ((a, b):rest) =
  if (b, a) `elem` prev then removeDups prev rest
  else removeDups (Set.insert (a, b) prev) rest

graphviz :: [Edge] -> String
graphviz = unlines . map (\(a, b) -> "  " ++ a ++ " -- " ++ b ++ ";")

removeEdge :: Graph -> (String, String) -> Graph
removeEdge g (n1, n2) =
  let g' = Map.adjust (delete n2) n1 g
  in  Map.adjust (delete n1) n2 g'

connectedSize :: Graph -> String -> Int
connectedSize g = Set.size . dfs g

dfs :: Graph -> String -> Set.Set String
dfs g start = dfs' g Set.empty [start]
  where
    dfs' :: Graph -> Set.Set String -> [String] -> Set.Set String
    dfs' _ visited [] = visited
    dfs' graph visited (x:xs)
      | x `Set.member` visited = dfs' graph visited xs
      | otherwise = dfs' graph (Set.insert x visited) (neighbors ++ xs)
      where neighbors = graph Map.! x
