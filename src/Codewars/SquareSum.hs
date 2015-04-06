module Codewars.SquareSum where
import Test.Hspec

squareSum :: [Integer] -> Integer
squareSum n = sum [x*x | x <- n]


---------------
-- Tests
---------------

test = hspec $ do
  describe "squareSum" $ do
    it "should work for some examples" $ do
      squareSum [1,2] `shouldBe` 5
      squareSum [5,3,4] `shouldBe` 50