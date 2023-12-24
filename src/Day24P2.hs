module Day24P2 (main) where

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

-- Pipe the output of this into z3
main :: IO ()
main = do
  input <- lines <$> getContents
  let hail = map parse input
  putStrLn "(declare-const px Real)"
  putStrLn "(declare-const py Real)"
  putStrLn "(declare-const pz Real)"
  putStrLn "(declare-const vx Real)"
  putStrLn "(declare-const vy Real)"
  putStrLn "(declare-const vz Real)"
  mapM_ putStrLn $ zipWith toAssertion (take 3 hail) [1..] 
  putStrLn "(check-sat)"
  putStrLn "(get-value ((+ px py pz)))"
  putStrLn "(exit)"

parse :: String -> Hail
parse str = case (map read . words . filter (`notElem` ",@")) str of
  [a, b, c, d, e, f] -> Hail a b c d e f
  _ -> error "No parse"

toAssertion :: Hail -> Int -> String
toAssertion (Hail hpx hpy hpz hvx hvy hvz) t = unlines
  [ "(declare-const t" ++ show t ++ " Int)"
  , "(assert (>= t" ++ show t ++ " 0))"
  , "(assert (= (+ " ++ show hpxi ++ " (* " ++ show hvxi ++ " t" ++ show t ++ ")) (+ px (* vx t" ++ show t ++ "))))"
  , "(assert (= (+ " ++ show hpyi ++ " (* " ++ show hvyi ++ " t" ++ show t ++ ")) (+ py (* vy t" ++ show t ++ "))))"
  , "(assert (= (+ " ++ show hpzi ++ " (* " ++ show hvzi ++ " t" ++ show t ++ ")) (+ pz (* vz t" ++ show t ++ "))))"
  ]
  where hpxi = floor hpx :: Int
        hpyi = floor hpy :: Int
        hpzi = floor hpz :: Int
        hvxi = floor hvx :: Int
        hvyi = floor hvy :: Int
        hvzi = floor hvz :: Int
