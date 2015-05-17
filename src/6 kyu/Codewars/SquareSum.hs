{- http://www.codewars.com/kata/515e271a311df0350d00000f "6 kyu","Square(n) Sum","515e271a311df0350d00000f"

Complete the squareSum method so that it squares each number passed into it and then sums the results together.

For example:

squareSum([1, 2, 2]); // should return 9
-}

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
