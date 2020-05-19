{-# LANGUAGE TupleSections #-}

module NigNagNug where

import           Control.Arrow (second)
import           Control.Monad (replicateM)
import           Data.List (maximumBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord (comparing)
import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

newtype Hand = Hand Int
  deriving (Eq, Ord, Show)

-- Game rules

compareHands :: Hand -> Hand -> Ordering
compareHands (Hand a) (Hand b)
  | a == b     = EQ
  | a - 1 == b = LT
  | a + 1 == b = GT
  | a > b      = GT
  | otherwise  = LT

points :: Hand -> Int
points (Hand 1) = 2
points (Hand _) = 1

beats :: Hand -> Hand -> Bool
beats hand1 hand2 =
  compareHands hand1 hand2 == GT

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
  where
    winner :: (Hand, Hand) -> [(Hand, Int)]
    winner (hand1, hand2) = case compareHands hand1 hand2 of
      GT -> [(hand2, points hand2)]
      LT -> [(hand1, points hand1)]
      EQ -> []

-- Fixed frequency strategy

frequencyMapGen :: Ord a => Map a Int -> Gen a
frequencyMapGen = QC.frequency . map gen . Map.toList
  where
    gen :: (a, Int) -> (Int, Gen a)
    gen (hand, score) = (score, pure hand)

-- Multi-strategy tournament

newtype Player = Player Int
  deriving (Eq, Ord, Show)

simple :: Map Player (Gen Hand)
simple = Map.fromList
  [ (Player 1, uniformHandGen)
  , (Player 2, uniformGameGen 10000 >>= frequencyMapGen)
  ]

tournament :: Int -> Map Player (Gen Hand) -> Gen (Map Player Int)
tournament n = fmap (Map.unionsWith (+)) . replicateM n . roundM

roundM :: Map Player (Gen Hand) -> Gen (Map Player Int)
roundM = fmap decideWinners . traverse go . Map.assocs
  where
    go :: (Player, Gen Hand) -> Gen (Player, Hand)
    go (player, handGen) = fmap (player,) handGen

decideWinners :: [(Player, Hand)] -> Map Player Int
decideWinners outcomes =
  Map.fromListWith (+) $
  map (second points) $
  filter isWinner outcomes
  where
    isWinner :: (Player, Hand) -> Bool
    isWinner (player, hand) =
      any (isBetter hand) outcomes &&
      all (not . isWorse hand) outcomes

    isBetter :: Hand -> (Player, Hand) -> Bool
    isBetter hand1 (_, hand2) = hand1 `beats` hand2

    isWorse :: Hand -> (Player, Hand) -> Bool
    isWorse hand1 (_, hand2) = hand2 `beats` hand1
