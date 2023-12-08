module Day07 (main) where

import Data.List (elemIndex, sortBy, sortOn)
import Data.Ord

main :: IO ()
main = do
  input <- map words . lines <$> getContents
  let hands  = map (Hand . head) input
      hands2 = map (Hand2 . head) input
      bids   = map (read . (!! 1)) input
  putStrLn $ "Part 1: " ++ show (solve hands bids)
  putStrLn $ "Part 2: " ++ show (solve hands2 bids)

solve :: Ord a => [a] -> [Integer] -> Integer
solve hands bids = sum $ map (\((_, b), r) -> b * r) orderedHands
  where orderedHands = zip (sortOn fst (zip hands bids)) [1..]

compareHands :: AHand a => String -> a -> a -> Ordering
compareHands cardOrder a b
  | hta == htb = mconcat $ zipWith (comparing (`elemIndex` cardOrder)) sa sb 
  | otherwise  = Data.Ord.compare hta htb
  where hta = handType a
        htb = handType b
        sa  = contents a
        sb  = contents b

data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
  deriving (Eq, Ord, Show)

class AHand a where
  contents  :: a -> String
  handType  :: a -> HandType

-- Hand type for part 1
newtype Hand = Hand String deriving (Eq, Show)

instance AHand Hand where
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
  contents (Hand s) = s

instance Ord Hand where compare = compareHands "23456789TJQKA"

-- Hand type for part 2
newtype Hand2 = Hand2 String deriving (Eq, Show)

instance AHand Hand2 where
  handType (Hand2 s) = maximum $ map (handType . Hand . try) "123456789TQKA"
    where try r = map (\x -> if x == 'J' then r else x) s
  contents (Hand2 s) = s

instance Ord Hand2 where compare = compareHands "J23456789TQKA"

