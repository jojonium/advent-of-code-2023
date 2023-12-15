module Day15 (main) where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Char (ord)
import Control.Arrow (second)
import qualified Data.Map as Map

type Lenses = [(String, Int)]
type Boxes  = Map.Map Int Lenses

main :: IO ()
main = do
  input <- map (filter (/='\n')) . splitOn "," <$> getContents
  putStrLn $ "Part 1: " ++ show (sum (map hash input))
  let p2 = foldl' operate Map.empty input
  putStrLn $ "Part 2: " ++ show (focusingPower p2)

hash :: String -> Int
hash = foldl' (\i c -> ((i + ord c) * 17) `mod` 256) 0

operate :: Boxes -> String -> Boxes
operate boxes inst = case break (`elem` "=-") inst of
  (label, "-") ->
    let boxN = hash label
        box  = Map.findWithDefault [] boxN boxes
        box' = filter ((/=label) . fst) box
    in  Map.insert boxN box' boxes
  (label, '=':n)  -> 
    let boxN   = hash label
        box    = Map.findWithDefault [] boxN boxes
        (a, b) = (second (drop 1)  . break ((==label) . fst)) box
        box'   = a ++ (label, read n) : b
    in  Map.insert boxN box' boxes
  _ -> error "Invalid instruction!"

focusingPower :: Boxes -> Int
focusingPower boxes = sum $ Map.mapWithKey boxPower boxes
  where boxPower n = sum . zipWith (curry lensPower) [1 .. ]
          where lensPower (s, (_, f)) = (n + 1) * s * f
