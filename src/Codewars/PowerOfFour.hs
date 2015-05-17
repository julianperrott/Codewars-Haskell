{- http://www.codewars.com/kata/544d114f84e41094a9000439

Write a method that returns true if a given parameter is a power of 4, and false if it's not. If parameter is not an Integer (eg String, Array) method should return false as well.

Examples

powerOf4(1024) // returns true
powerOf4(44) // returns false
powerOf4("not a positive integer") // returns false
-}
module Codewars.PowerOfFour where
import Test.Hspec
import Test.QuickCheck

isPowerOf4 :: Integral n => n -> Bool
isPowerOf4 n
    | n == 1 = True
    | mod n 4 > 0 = False
    | otherwise = isPowerOf4 $ n `div` 4


test = hspec $ do
  describe "isPowerOf4" $ do
    it "should work for some examples" $ do
      isPowerOf4 1024 `shouldBe` True
      isPowerOf4  102 `shouldBe` False
      isPowerOf4   64 `shouldBe` True
    it "should work for random powers of four" $ do
      property $ forAll (arbitrary `suchThat` (>=0)) $ \x ->
        isPowerOf4 (4^(x :: Int)) `shouldBe` True
    it "should work for random non powers of four" $ do
      property $ forAll (arbitrary `suchThat` (>=0)) $ \x ->
        isPowerOf4 ((4^(x :: Int)) + 1) `shouldBe` False
