{- http://www.codewars.com/kata/5436f26c4e3d6c40e5000282 "7 kyu","SequenceSum","5436f26c4e3d6c40e5000282"

Sum of 'n' Numbers

sum_of_n (or SequenceSum.sumOfN in Java) takes an integer n and returns a List of length abs(n) + 1. The List contains the numbers in the arithmetic series produced by taking the sum of the consecutive integer numbers from 0 to n inclusive.

n can also be 0 or a negative value.
Example:

5 -> [0, 1, 3, 6, 10, 15]

-5 -> [0, -1, -3, -6, -10, -15]

7 -> [0, 1, 3, 6, 10, 15, 21, 28]
-}

module Codewars.SequenceSum where
import Test.Hspec

sumIt :: Int -> Int
sumIt n = sum[0..n]

sumOfN :: Int -> [Int]
sumOfN n
    | n < 0 = map negate (sumOfN (abs(n)))
    | otherwise = map sumIt [0..n]

-- [negate is the function applied by Haskell's only prefix operator, minus; we can't call it (-), because that is the subtraction function, so this name is provided instead.
-- For example, -x*y is equivalent to negate (x*y). (Prefix minus has the same syntactic precedence as infix minus, which, of course, is lower than that of multiplication.)]

---------------
-- Tests
---------------

testSequenceSum = hspec $ do
  describe "sumOfN" $ do
    it "should work for some examples" $ do
      sumOfN 3    `shouldBe` [0, 1, 3, 6]
      sumOfN 1    `shouldBe` [0, 1]
      sumOfN 0    `shouldBe` [0]
      sumOfN (-4) `shouldBe` [0, -1, -3, -6, -10]
