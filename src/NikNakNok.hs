
module NikNakNok where

import Data.Map (Map)
import qualified Data.Map as Map

import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

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
points (Hand 1) = 2
points (Hand _) = 1

winner :: (Hand, Hand) -> [(Hand, Int)]
winner (hand1, hand2) = case compareHands hand1 hand2 of
  GT -> [(hand2, points hand2)]
  LT -> [(hand1, points hand1)]
  EQ -> []

-- Uniform strategy

uniformHandGen :: Gen Hand
uniformHandGen = Hand <$> QC.elements [1..4]

pairGen :: Gen a -> Gen b -> Gen (a, b)
pairGen aGen bGen = (,) <$> aGen <*> bGen

uniformMatchGen :: Gen (Hand, Hand)
uniformMatchGen = pairGen uniformHandGen uniformHandGen

uniformGameGen :: Int -> Gen (Map Hand Int)
uniformGameGen n = do
  games <- QC.vectorOf n uniformMatchGen
  let wins = concatMap winner games
  pure (Map.fromListWith (+) wins)

-- Fixed frequency strategy

frequencyMapGen :: Ord a => Map a Int -> Gen a
frequencyMapGen = QC.frequency . map gen . Map.toList
  where
    gen :: (a, Int) -> (Int, Gen a)
    gen (hand, score) = (score, pure hand)

frequencyHandGen :: Map Hand Int -> Gen Hand
frequencyHandGen = frequencyMapGen

-- Multi-strategy tournament

oneHandGen :: Gen Hand
oneHandGen = uniformGameGen 10000 >>= frequencyHandGen

newtype Player = Player Int
  deriving (Eq, Ord, Show)

tournament :: Int -> Map Player (Gen Hand) -> Gen (Map Player Int)
tournament n competitors = undefined
