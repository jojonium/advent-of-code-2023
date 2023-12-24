module Day24 (main) where

import Data.List (tails)
import Data.Maybe (mapMaybe)

data Hail = Hail
  { _px :: Double
  , _py :: Double
  , _pz :: Double
  , _vx :: Double
  , _vy :: Double
  , _vz :: Double
  } deriving (Eq, Show)

main :: IO ()
main = do
  hail <- map parse . lines <$> getContents
  let p1 = length (mapMaybe valid [(a, b) | (a:bs) <- tails hail, b <- bs])
  putStrLn $ "Part 1: " ++ show p1

parse :: String -> Hail
parse str = case (map read . words . filter (`notElem` ",@")) str of
  [a, b, c, d, e, f] -> Hail a b c d e f
  _ -> error "No parse"

valid :: (Hail, Hail) -> Maybe (Double, Double)
valid (ha, hb) =
  let ma = _vy ha / _vx ha
      mb = _vy hb / _vx hb
      ba = _py ha - ma * _px ha
      bb = _py hb - mb * _px hb
  in case inter (ma, ba) (mb, bb) of
    Nothing -> Nothing
    Just (x, y) ->
      let opa = if _vx ha < 0 then (<) else (>)
          opb = if _vx hb < 0 then (<) else (>)
      in if x `opa` _px ha && x `opb` _px hb &&
            x >= 200000000000000 && x <= 400000000000000 &&
            y >= 200000000000000 && y <= 400000000000000
         then Just (x, y) else Nothing

inter :: (Double, Double) -> (Double, Double) -> Maybe (Double, Double)
inter (m1, b1) (m2, b2)
  | m1 == m2 =  Nothing
  | otherwise = Just (x, y)
  where x = (b2 - b1) / (m1 - m2)
        y = m1 * ((b2 - b1) / (m1 - m2)) + b1

