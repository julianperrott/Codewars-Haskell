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