
module Main where

import qualified Data.Map as Map
import           Test.Hspec
import           Test.QuickCheck

import           NigNagNug

main :: IO ()
main = hspec specs

specs :: Spec
specs = do
  describe "compareHands" $ do
    describe "Hand 1" $ do
      Hand 1 `shouldTieWith` Hand 1
      Hand 1 `shouldBeat` Hand 2
      Hand 1 `shouldLoseTo` Hand 3
      Hand 1 `shouldLoseTo` Hand 4

    describe "Hand 2" $ do
      Hand 2 `shouldLoseTo` Hand 1
      Hand 2 `shouldTieWith` Hand 2
      Hand 2 `shouldBeat` Hand 3
      Hand 2 `shouldLoseTo` Hand 4

    describe "Hand 3" $ do
      Hand 3 `shouldBeat` Hand 1
      Hand 3 `shouldLoseTo` Hand 2
      Hand 3 `shouldTieWith` Hand 3
      Hand 3 `shouldBeat` Hand 4

    describe "Hand 4" $ do
      Hand 4 `shouldBeat` Hand 1
      Hand 4 `shouldBeat` Hand 2
      Hand 4 `shouldLoseTo` Hand 3
      Hand 4 `shouldTieWith` Hand 4

  describe "decideWinners" $ do
    it "has no winners with no players" $
      decideWinners [] `shouldBe` Map.empty

    it "picks the best hand with two players" $
      property $
        forAll uniformHandGen $ \hand1 ->
        forAll uniformHandGen $ \hand2 -> do
          let competitors =
                [ (Player 1, hand1), (Player 2, hand2) ]

          let expected = case compareHands hand1 hand2 of
                GT -> [(Player 1, points hand1)]
                LT -> [(Player 2, points hand2)]
                EQ -> []

          decideWinners competitors === Map.fromList expected

-- Unit-test helpers

shouldBeat hand1 hand2 =
  it (show hand1 <> " `compareHands` " <> show hand2) $
    (hand1 `compareHands` hand2) `shouldBe` GT

shouldTieWith hand1 hand2 =
  it (show hand1 <> " `compareHands` " <> show hand2) $
    (hand1 `compareHands` hand2) `shouldBe` EQ

shouldLoseTo hand1 hand2 =
  it (show hand1 <> " `compareHands` " <> show hand2) $
    (hand1 `compareHands` hand2) `shouldBe` LT
