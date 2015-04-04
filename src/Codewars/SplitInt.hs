module Codewars.SplitInt where
import Test.Hspec

splitInteger :: Int -> Int -> [Int]
splitInteger value parts = replicate (parts-r) v ++ replicate r (v+1)
    where r = value `rem` parts
          v = value `quot` parts


test = hspec $ do
  describe "Simple Functionality" $ do 
    it "10 by 1 should be [10]" $ do
      splitInteger 10 1 `shouldBe` [10]
    it "2 by 2 should be [1, 1]" $ do
      splitInteger 2 2 `shouldBe` [1, 1]
    it "20 by 5 should be [4, 4, 4, 4, 4]" $ do
      splitInteger 20 5 `shouldBe` [4, 4, 4, 4, 4]
      
  describe "Uneven Tests" $ do 
    it "20 by 6 should be [3, 3, 3, 3, 4, 4]" $ do
      splitInteger 20 6 `shouldBe` [3, 3, 3, 3, 4, 4]
    it "11 by 3 should be [1, 1]" $ do
      splitInteger 11 3 `shouldBe` [3, 4, 4]