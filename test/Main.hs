
module Main where

import Test.Hspec

import NigNagNug

main :: IO ()
main = hspec specs

--shouldBeat :: Hand -> Hand -> Expectation
shouldBeat hand1 hand2 =
  it (show hand1 <> " `compareHands` " <> show hand2) $
    (hand1 `compareHands` hand2) `shouldBe` GT

--shouldTieWith :: Hand -> Hand -> Expectation
shouldTieWith hand1 hand2 =
  it (show hand1 <> " `compareHands` " <> show hand2) $
    (hand1 `compareHands` hand2) `shouldBe` EQ

--shouldLoseTo :: Hand -> Hand -> Expectation
shouldLoseTo hand1 hand2 =
  it (show hand1 <> " `compareHands` " <> show hand2) $
    (hand1 `compareHands` hand2) `shouldBe` LT

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
