{-# LANGUAGE TupleSections #-}
module Day22 (main) where

import Data.List.Split (splitOneOf)
import Data.List (sortOn, nub)
import Control.Monad (replicateM)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Debug.Trace (traceShow)

data Coord = Coord {_x :: Int, _y :: Int, _z :: Int} deriving (Eq, Ord, Show)
data Block = Block {_s :: Coord, _e :: Coord, _label :: String} deriving (Eq, Show)
type BlockMap = Map.Map String [String]
type Settled  = Map.Map Coord String

main :: IO ()
main = do
  let labels = [1..] >>= flip replicateM ['A' .. 'Z']
  blocks <- sortOn (_z . _s) . map (uncurry parse) . (`zip` labels) . lines <$> getContents
  let initMap = Map.fromList (map ((, []) . _label) blocks)
      (supports, supportedBy, settled) = fall' initMap initMap Map.empty blocks
      loneSupports    = (nub . concat . filter ((== 1) . length . nub) . map snd . Map.toList) supportedBy
      disintegratable = (filter (`notElem` loneSupports)  . map _label) blocks
  putStrLn $ "Part 1: " ++ show (length disintegratable)

parse :: String -> String -> Block
parse str label = case map read (splitOneOf ",~" str) of
  [a, b, c, d, e, f] -> Block (Coord a b c) (Coord d e f) label
  _ -> error "No parse"

pointsBelow :: Block -> [Coord]
pointsBelow (Block (Coord sx sy sz) (Coord ex ey ez) _) =
  [Coord x y (z - 1) | x <- [sx .. ex], y <- [sy .. ey], z <- [sz .. ez]]

blocksThatContain :: Settled -> [Coord] -> [String]
blocksThatContain settled = mapMaybe (`Map.lookup` settled)

moveDown :: Block -> Block
moveDown (Block (Coord sx sy sz) (Coord ex ey ez) l) =
  Block (Coord sx sy (sz - 1)) (Coord ex ey (ez - 1)) l

settle :: Settled -> Block -> Settled
settle settled (Block (Coord sx sy sz) (Coord ex ey ez) l) =
  let coords = [Coord x y z | x <- [sx .. ex], y <- [sy .. ey], z <- [sz .. ez]]
  in  foldr (`Map.insert` l) settled coords

fall' :: BlockMap -> BlockMap -> Settled -> [Block] -> (BlockMap, BlockMap, Settled)
fall' supports supportedBy settled [] = (supports, supportedBy, settled)
fall' supports supportedBy settled (f:falling)
  | _z (_s f) == 1 = fall' supports supportedBy (settle settled f) falling
  | not (null blocksBelow) = fall' supports' supportedBy' (settle settled f) falling
  | otherwise = fall' supports supportedBy settled (moveDown f : falling)
  where blocksBelow  = blocksThatContain settled (pointsBelow f)
        supports'    = foldr (\s m -> Map.insertWith (++) s [_label f] m) supports blocksBelow
        supportedBy' = foldr (\s m -> Map.insertWith (++) (_label f) [s] m) supportedBy blocksBelow
