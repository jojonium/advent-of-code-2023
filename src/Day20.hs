{-# LANGUAGE TupleSections #-}
module Day20 (main) where

import qualified Data.Map as Map
import Control.Monad.State

data Pulse = High | Low deriving (Eq, Show)
type ModMap = Map.Map String Module
type ModuleState = (ModMap, Int, Int, Map.Map String Int)

data Module = 
  Broadcast [String] |
  FlipFlop { _fOn :: Bool , _fOutputs :: [String] } |
  Conjunct { _cMem :: Map.Map String Pulse, _cOutputs :: [String] }
  deriving (Show)

-- source of pulse -> pulse -> my name -> my Module -> (new state, [(from, to, pulse)])
process :: String -> Pulse -> String -> Module -> (Module, [(String, String, Pulse)])
process _ p me b@(Broadcast outputs) = (b, map (me, , p) outputs)
process _ High _ ff@(FlipFlop _ _) = (ff, [])
process _ Low me ff@(FlipFlop False outputs) = (ff {_fOn = True}, map (me, , High) outputs)
process _ Low me ff@(FlipFlop True  outputs) = (ff {_fOn = False}, map (me, , Low) outputs)
process src p me c@(Conjunct mem outputs) = (c {_cMem = mem'}, map (me, , o) outputs)
  where mem' = Map.insert src p mem
        o    = if all (==High) (Map.elems mem') then Low else High

main :: IO ()
main = do
  modMap <- initConjuncts . parse . lines <$> getContents
  putStrLn $ "Part 1: " ++ show (uncurry (*) (pushButton modMap 1000))
  let final    = (fst . head . filter (sendsTo "rx" . snd) . Map.assocs) modMap
      targetNs = (map fst . filter (sendsTo final . snd) . Map.assocs) modMap
      targets  = Map.fromList (map (, 0) targetNs)
  putStrLn $ "Part 2: " ++ show (part2 modMap targets 1)

parse :: [String] -> ModMap
parse = foldr helper Map.empty
  where helper l modMap = case head l of 
                  '%' -> let m = FlipFlop False outputs
                         in  Map.insert (drop 1 (head ws)) m modMap
                  '&' -> let m = Conjunct Map.empty outputs
                         in  Map.insert (drop 1 (head ws)) m modMap
                  _ ->   Map.insert (head ws) (Broadcast outputs) modMap
          where ws = words l
                outputs = map (filter (/= ',')) (drop 2 ws)

initConjuncts :: ModMap -> ModMap
initConjuncts modMap = foldr helper modMap conjuncts
  where conjuncts = map fst (filter (isConjunct . snd) (Map.assocs modMap))
        helper cn m = Map.adjust adjFunc cn m
          where adjFunc (Conjunct mem os) = Conjunct (foldr (`Map.insert` Low) mem sources) os
                adjFunc _ = error "Calling adjFunc on a non-conjunct module"
                sources = map fst (filter (sendsTo cn . snd) (Map.assocs m))

isConjunct :: Module -> Bool
isConjunct (Conjunct _ _) = True
isConjunct _ = False

sendsTo :: String -> Module -> Bool
sendsTo str (Broadcast os) = str `elem` os
sendsTo str (FlipFlop _ os) = str `elem` os
sendsTo str (Conjunct _ os) = str `elem` os

sendPulses :: Int -> [(String, String, Pulse)] -> State ModuleState ModuleState
sendPulses _ [] = do get
sendPulses bps ((from, to, p):rest) = do
  (modMap, lows, highs, cycles) <- get
  let (modMap', next, cycles') = case Map.lookup to modMap of
        Nothing -> (modMap, [], cycles)
        Just aMod -> let (mod', ns) = process from p to aMod
                         cyc = if from `Map.member` cycles && p == High
                               then Map.insert from bps cycles
                               else cycles
                     in  (Map.insert to mod' modMap, ns, cyc)
      lows'  = if p == Low  then lows  + 1 else lows
      highs' = if p == High then highs + 1 else highs
  put (modMap', lows', highs', cycles')
  sendPulses bps (rest ++ next)

pushButton :: ModMap -> Int -> (Int, Int)
pushButton _ 0 = (0, 0)
pushButton modMap n = (lows + nextLows, highs + nextHighs)
  where initPulse = [("button", "broadcaster", Low)]
        (modMap', lows, highs, _) = evalState (sendPulses 0 initPulse) (modMap, 0, 0, Map.empty)
        (nextLows, nextHighs)  = pushButton modMap' (n - 1)

part2 :: ModMap -> Map.Map String Int -> Int -> Int
part2 modMap cycles n = 
  let initPulse = [("button", "broadcaster", Low)]
      (modMap', _, _, cycles') = evalState (sendPulses n initPulse) (modMap, 0, 0, cycles)
  in  if all (> 0) (Map.elems cycles') then foldr1 lcm (Map.elems cycles')
      else part2 modMap' cycles' (n + 1)
