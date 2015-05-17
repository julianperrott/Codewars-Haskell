{- http://www.codewars.com/kata/55252a50de8b4bac00000805

Write a function to multiply a number (x) by a given number (y) a certain number of times (n). The results are to be returned in an array.

eg.

multiplyBy(2, 4, 6);
The output is: [8, 32, 128, 512, 2048, 8192]

NB: all arguments (x,y and n) will always be integers. Times (n) will always be a positive integer.

-}

module Codewars.MultiplyBy where
import Test.Hspec
import Test.QuickCheck

multiplyBy :: Integer -> Integer -> Int -> [Integer]
multiplyBy x y 0 = []
multiplyBy x y 1 = [x*y]
multiplyBy x y n = [x*y] ++ multiplyBy (x*y) y (n-1)





test = hspec $ do
  describe "multiplyBy" $ do
    it "should work for some simple tests" $ do
      multiplyBy 1 1 1 `shouldBe` [1]
      multiplyBy 2 4 6 `shouldBe` [8, 32, 128, 512, 2048, 8192]

    it "should work for x = 0" $ do
      property $ \y (Positive n) ->
        multiplyBy 0 y n `shouldBe` replicate n 0

    it "should work for y = 1" $ do
      property $ \x (Positive n) ->
        multiplyBy x 1 n `shouldBe` replicate n x
