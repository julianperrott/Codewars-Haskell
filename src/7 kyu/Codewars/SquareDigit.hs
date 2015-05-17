{- http://www.codewars.com/kata/546e2562b03326a88e000020 "7 kyu","Square Every Digit","546e2562b03326a88e000020"

Welcome. In this kata, you are asked to square every digit of a number.

For example, if we run 9119 through the function, 811181 will come out.

Note: The function accepts an integer and returns an integer
-}

module Codewars.SquareDigit where
import Data.Char
import Test.QuickCheck
import Test.Hspec

squareDig :: Char -> [Char]
squareDig n = show $ (digitToInt n) ^2

squareDigit :: Int -> Int
squareDigit n
    | n < 1 = -1 * (squareDigit $ negate n)
    | otherwise =  read $ concatMap squareDig $ show n


---------------
-- Tests
---------------

test = hspec $ do
  describe "Testing solution:" $ do
    it "Should double a positive integer:" $ do
      squareDigit 9119 `shouldBe` 811181
      squareDigit (-1) `shouldBe` (-1)
      squareDigit (-9119) `shouldBe` (-811181)
    it "squareDig:" $ do
        squareDig '2' `shouldBe` "4"
        squareDig '9' `shouldBe` "81"
