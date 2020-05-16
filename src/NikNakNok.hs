
module NikNakNok where

import Data.Map (Map)
import qualified Data.Map as Map
import Test.QuickCheck

newtype Hand = Hand Int
  deriving (Eq, Ord, Show)

-- Game rules

compareHands :: Hand -> Hand -> Ordering
compareHands (Hand a) (Hand b)
  | a == b     = EQ
  | a + 1 == b = GT
  | a > b      = GT
  | otherwise  = LT

points :: Hand -> Int
points (Hand 2) = 2
points (Hand _) = 1

winner :: (Hand, Hand) -> [(Hand, Int)]
winner (hand1, hand2) = case compareHands hand1 hand2 of
  GT -> [(hand2, points hand2)]
  LT -> [(hand1, points hand1)]
  EQ -> []

-- Generator

choiceGen :: Gen Hand
choiceGen = Hand <$> elements [1..4]

pairGen :: Gen (Hand, Hand)
pairGen = (,) <$> choiceGen <*> choiceGen

uniformGameGen :: Int -> Gen (Map Hand Int)
uniformGameGen n = do
  games <- vectorOf n pairGen
  let wins = concatMap winner games
  pure (Map.fromListWith (+) wins)
