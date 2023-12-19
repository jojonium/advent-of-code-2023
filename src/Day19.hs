module Day19 (main) where

import qualified Data.Map as Map
import Data.List.Split (splitOn, splitOneOf)

type Range     = (Integer, Integer) -- inclusive on both ends
type Workflows = Map.Map String String
type PartMap   = Map.Map Char Int
type RangeMap  = Map.Map Char Range

main :: IO ()
main = do
  [ws, ps] <- map lines . splitOn "\n\n" <$> getContents
  let workflows = parseWorkflows ws
      parts     = map partMap ps
      rangeMap  = foldr (\x m -> Map.insert x (1, 4000) m) Map.empty "xmas"
  putStrLn $ "Part 1: " ++ show 
    (sum (map (sortPart workflows (workflows Map.! "in")) parts))
  let paths = part2 workflows rangeMap (splitOn "," (workflows Map.! "in"))
  putStrLn $ "Part 2: " ++ show
    (sum (map (product . map ((+ 1) . uncurry subtract) . Map.elems) paths))

parseWorkflows :: [String] -> Workflows
parseWorkflows = foldr helper Map.empty
  where helper str m = case splitOneOf "{}" str of
          (a:b:_) -> Map.insert a b m
          _ -> error $ "Failed parse: " ++ str

partMap :: String -> PartMap
partMap str = foldr helper Map.empty attributes
  where attributes = splitOn "," $ filter (`notElem` "{}") str
        helper attr m = let (k, v) = break (=='=') attr
                        in Map.insert (head k) (read (tail v)) m

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
        operator = case r !! 1 of '<' -> (<); '>' -> (>); _ -> error "No parse"

part2 :: Workflows -> RangeMap -> [String] -> [RangeMap]
part2 _ _ [] = error "Reached end of rules"
part2 _ rangeMap ("A":_) = [rangeMap]
part2 _ _ ("R":_) = []
part2 wkfls rangeMap (r:rs)
  | ':' `notElem` r = part2 wkfls rangeMap (splitOn "," (wkfls Map.! r))
  | otherwise = case r !! 1 of
    '<' -> let successM = Map.insert partCat (l, val - 1) rangeMap
               failureM = Map.insert partCat (val, u) rangeMap
           in  concat ([part2 wkfls successM successR  | l < val] ++
                 [part2 wkfls failureM rs | u >= val])
    '>' -> let successM = Map.insert partCat (val + 1, u) rangeMap
               failureM = Map.insert partCat (l, val) rangeMap
           in  concat ([part2 wkfls successM successR  | u > val] ++
                 [part2 wkfls failureM rs | l <= val])
    _   -> error "No parse"
  where partCat  = head r
        (n, s)   = break (==':') (drop 2 r)
        val      = read n
        (l, u)   = rangeMap Map.! partCat
        successR = case drop 1 s of 
          "A" -> ["A"]; "R" -> ["R"]; wfk -> splitOn "," (wkfls Map.! wfk)

