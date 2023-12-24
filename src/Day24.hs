module Day24 (main) where

import Data.List (tails)
import Control.Arrow ((***))
import Data.Maybe (mapMaybe)

data Hail = Hail
  { _px :: Double
  , _py :: Double
  , _pz :: Double
  , _vx :: Double
  , _vy :: Double
  , _vz :: Double
  } deriving (Eq, Show)

data Line = Line {_m :: Double, _b :: Double} -- y = mx + b
  deriving (Eq, Show)

main :: IO ()
main = do
  hail <- map parse . lines <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 hail)

part1 :: [Hail] -> Int
part1 hail = length (mapMaybe valid [(a, b) | (a:bs) <- tails hail, b <- bs])
  where valid :: (Hail, Hail) -> Maybe (Double, Double)
        valid (ha, hb) =
          let (la, lb) = (toLine *** toLine) (ha, hb)
          in case inter la lb of
            Nothing -> Nothing
            Just (x, y) ->
              let opa = if _vx ha < 0 then (<) else (>)
                  opb = if _vx hb < 0 then (<) else (>)
              in if x `opa` _px ha && x `opb` _px hb &&
                    x >= 200000000000000 && x <= 400000000000000 &&
                    y >= 200000000000000 && y <= 400000000000000
                 then Just (x, y) else Nothing

parse :: String -> Hail
parse str = case (map read . words . filter (`notElem` ",@")) str of
  [a, b, c, d, e, f] -> Hail a b c d e f
  _ -> error "No parse"

toLine :: Hail -> Line
toLine (Hail px py pz vx vy vz) = Line m b
  where m =  vy / vx
        b = py - m * px

inter :: Line -> Line -> Maybe (Double, Double)
inter (Line m1 b1) (Line m2 b2)
  | m1 == m2 =  Nothing
  | otherwise = Just (x, y)
  where x = (b2 - b1) / (m1 - m2)
        y = m1 * ((b2 - b1) / (m1 - m2)) + b1
