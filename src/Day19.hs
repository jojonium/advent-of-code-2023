module Day19 (main) where

import qualified Data.Map as Map
import Data.List.Split (splitOn, splitOneOf)
import Debug.Trace (traceShow)

type Workflows = Map.Map String String
type PartMap   = Map.Map Char Int

main :: IO ()
main = do
  [ws, ps] <- map lines . splitOn "\n\n" <$> getContents
  let workflows = parseWorkflows ws
      parts     = map partMap ps
      p1 = sum $ map (sortPart workflows (workflows Map.! "in")) parts
  putStrLn $ "Part 1: " ++ show p1

parseWorkflows :: [String] -> Workflows
parseWorkflows = foldr helper Map.empty
  where helper str m = case splitOneOf "{}" str of
          (a:b:_) -> Map.insert a b m
          _ -> error $ "Failed parse: " ++ str

sortPart :: Workflows -> String -> PartMap -> Int
sortPart workflows rules part = case matchRule part (splitOn "," rules) of
  "R" -> 0
  "A" -> sum (Map.elems part)
  w   -> sortPart workflows (workflows Map.! w) part

matchRule :: PartMap -> [String] -> String
matchRule _ [] = error "Reached end of rules"
matchRule part (r:rs)
  | ':' `notElem` r = r
  | otherwise = if part Map.! partCat `operator` read n
                then drop 1 s
                else matchRule part rs
  where partCat  = head r
        (n, s)   = break (==':') (drop 2 r)
        operator = case r !! 1 of '<' -> (<); '>' -> (>); _ -> error "Failed parse"

partMap :: String -> PartMap
partMap str = foldr helper Map.empty attributes
  where attributes = splitOn "," $ filter (`notElem` "{}") str
        helper attr m = let (k, v) = break (=='=') attr
                        in Map.insert (head k) (read (tail v)) m
