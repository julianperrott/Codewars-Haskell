module Codewars.Weights where
import           Test.Hspec

-- | Takes a bottleWeight and scale of
--   the bottle to its contents

scaleValue :: String -> Int
scaleValue str = read (words str !! 0)

contentWeight :: Int -> String -> Int
contentWeight weight str 
    | last (words str) == "larger" = weight * (scaleValue str) `div` ((scaleValue str)+1)
    | otherwise = weight `div` ((scaleValue str)+1)

---------------
-- Tests
---------------

test = hspec $ do
  describe "contentWeight" $ do
    it "should work for larger contents" $ do
      contentWeight 120 "2 times larger" `shouldBe`  80
      contentWeight 180 "2 times larger" `shouldBe` 120
    it "should work for smaller contents" $ do
      contentWeight 120 "2 times smaller" `shouldBe`  40
      contentWeight 300 "2 times smaller" `shouldBe` 100
    it "should work for words" $ do
      scaleValue "2 times smaller" `shouldBe` 2