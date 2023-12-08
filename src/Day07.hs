module Day07 (main) where

import Data.List (elemIndex, sortBy, sortOn)
import Data.Ord

main :: IO ()
main = do
  input <- map words . lines <$> getContents
  let hands  = map (Hand . head) input
      hands2 = map (Hand2 . head) input
      bids   = map (read . (!! 1)) input
  putStrLn $ "Part 1: " ++ show (part1 hands bids)
  putStrLn $ "Part 2: " ++ show (part2 hands2 bids)

newtype Hand = Hand String deriving (Eq, Show)

instance Ord Hand where
  compare a@(Hand sa) b@(Hand sb)
    | hta == htb = mconcat $ zipWith (comparing (`elemIndex` "123456789TJQKA")) sa sb 
    | otherwise  = compare hta htb
    where hta = handType a
          htb = handType b

data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
  deriving (Eq, Ord, Show)

handType :: Hand -> HandType
handType (Hand s) = case sortBy (comparing Down) counts of
    (5:_)       -> FiveKind
    (4:_)       -> FourKind
    (3:3:3:2:_) -> FullHouse
    (3:_)       -> ThreeKind
    (2:2:2:2:_) -> TwoPair
    (2:_)       -> OnePair
    (1:_)       -> HighCard
    _ -> error "Unknown hand type!"
  where counts = map (\x -> length (filter (==x) s)) s

part1 :: [Hand] -> [Integer] -> Integer
part1 hands bids = sum $ map (\((_, b), r) -> b * r) orderedHands
  where orderedHands = zip (sortOn fst (zip hands bids)) [1..]

-------------------------------------------------------------------------------

newtype Hand2 = Hand2 String deriving (Eq, Show)

instance Ord Hand2 where
  compare a@(Hand2 sa) b@(Hand2 sb)
    | hta == htb = mconcat $ zipWith (comparing (`elemIndex` "J123456789TQKA")) sa sb 
    | otherwise  = compare hta htb
    where hta = handType2 a
          htb = handType2 b

handType2 :: Hand2 -> HandType
handType2 (Hand2 s) = maximum $ map (handType . Hand . try) "123456789TQKA"
  where try r = map (\x -> if x == 'J' then r else x) s

part2 :: [Hand2] -> [Integer] -> Integer
part2 hands bids = sum $ map (\((_, b), r) -> b * r) orderedHands
  where orderedHands = zip (sortOn fst (zip hands bids)) [1..]
